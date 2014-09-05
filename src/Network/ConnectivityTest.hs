{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Network.ConnectivityTest
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
module Network.ConnectivityTest
(
-- * Connection Parameters
  TTL
, ConnectionType(..)
, defaultConnectionType
, parseConnectionType
, pConnectionType

, ConnectionParams(..)
, defaultConnectionParams
, pConnectionParams

-- * Check Connection Result
, ConnectionResult(..)
, ConnectionFailure(..)
, NotReachableDetail(..)

-- * Connection Check
, checkConnection
) where

import Control.Applicative
import Control.Error
import Control.Exception.Lifted
import Control.Lens hiding ((.=), act)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.Monoid
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable
import Data.Word

import Network.BSD (getProtocolNumber)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Options.Applicative as O

import System.Timeout

import Configuration.Utils

#if mingw32_HOST_OS == 1
import Win32Icmp
#else
import Data.Bits (complement)
#endif

-- -------------------------------------------------------------------------- --
-- Misc Utils

sshow ∷ (Show a, IsString b) ⇒ a → b
sshow = fromString . show

tryC ∷ (MonadIO m, Exception e) ⇒ (T.Text → e) → IO a → EitherT e m a
tryC failureConstr = fmapLT (failureConstr . sshow) . tryIO

d2N ∷ Iso' Double NominalDiffTime
d2N = iso realToFrac realToFrac

-- -------------------------------------------------------------------------- --
-- Time Measurment

getTime ∷ IO Double
getTime = realToFrac <$> getPOSIXTime

time ∷ IO a → IO (NominalDiffTime, a)
time act = do
  start ← getTime
  result ← act
  end ← getTime
  let !delta = end - start
  return (realToFrac delta, result)

timeT ∷ MonadIO m ⇒ m a → m (NominalDiffTime, a)
timeT act = do
  start ← liftIO getTime
  result ← act
  end ← liftIO getTime
  let !delta = end - start
  return (realToFrac delta, result)

timeoutT
    ∷ (MonadBaseControl IO m)
    ⇒ T.Text           -- ^ label
    → (T.Text → b)    -- ^ exception constructor
    → NominalDiffTime  -- ^ timeout
    → EitherT b m a    -- ^ action
    → EitherT b m a
timeoutT label exConstr t a = do
    r ← liftBaseWith $ \runInBase →
        timeout (round $ t * 1e6) (runInBase a)
    case r of
        Nothing → left $ exConstr $ label ⊕ " timed out after " ⊕ sshow t
        Just x → restoreM x

-- -------------------------------------------------------------------------- --
-- Parameters

type TTL = Int

-- | type of the IP connection
--
data ConnectionType
    = UDP
    | TCP
    | ICMP
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

defaultConnectionType ∷ ConnectionType
defaultConnectionType = TCP

instance ToJSON ConnectionType where
    toJSON UDP = "udp"
    toJSON TCP = "tcp"
    toJSON ICMP = "icmp"

instance FromJSON ConnectionType where
    parseJSON = withText "ConnectionType" $
        either (fail . T.unpack) return . parseConnectionType

parseConnectionType ∷ T.Text → Either T.Text ConnectionType
parseConnectionType t = case CI.mk (T.encodeUtf8 t) of
    "udp" → return UDP
    "tcp" → return TCP
    "icmp" → return ICMP
    _ → Left $ "unrecognized connection type: " ⊕ t

pConnectionType ∷ O.Parser ConnectionType
pConnectionType = option (eitherReader $ fmapL T.unpack . parseConnectionType . T.pack)
    × long "type"
    ⊕ help "connection type"
    ⊕ metavar "tcp|udp|icmp"
    ⊕ value TCP

-- | Parameters
--
data ConnectionParams = ConnectionParams
    { cpType ∷ !ConnectionType
    , cpHost ∷ !HostName
    , cpPort ∷ !Word16
    -- ^ ignored for connection type @icmp@
    , cpTtl ∷ !(Maybe Int)
    , cpTimeout ∷ !NominalDiffTime
    , cpTrace ∷ !Bool
    }
    deriving (Show, Eq, Ord, Typeable)

$(makeLensesFor
    [ ("cpType", "cpTypeL")
    , ("cpHost", "cpHostL")
    , ("cpPort", "cpPortL")
    , ("cpTtl", "cpTtlL")
    , ("cpTimeout", "cpTimeoutL")
    , ("cpTrace", "cpTraceL")
    ] ''ConnectionParams)

defaultConnectionParams ∷ ConnectionParams
defaultConnectionParams = ConnectionParams
    { cpType = defaultConnectionType
    , cpHost = "localhost"
    , cpPort = 80
    , cpTtl = Nothing
    , cpTimeout = 1
    , cpTrace = False
    }

instance ToJSON ConnectionParams where
    toJSON ConnectionParams{..} = object
        [ "type" .= cpType
        , "host" .= cpHost
        , "port" .= cpPort
        , "ttl" .= cpTtl
        , "timeout" .= (realToFrac cpTimeout ∷ Double)
        , "trace" .= cpTrace
        ]

instance FromJSON (ConnectionParams → ConnectionParams) where
    parseJSON = withObject "ConnectionsParams" $ \o → id
        <$< cpTypeL ..: "type" × o
        <*< cpHostL ..: "host" × o
        <*< cpPortL ..: "port" × o
        <*< cpTtlL ..: "ttl" × o
        <*< cpTimeoutL . from d2N ..: "timeout" × o
        <*< cpTraceL ..: "trace" × o
      where

pConnectionParams ∷ MParser ConnectionParams
pConnectionParams = id
    <$< cpTypeL .:: pConnectionType
    <*< cpHostL .:: strOption
        × long "host"
        ⊕ metavar "HOSTNAME"
        ⊕ help "host name"
    <*< cpPortL .:: option auto
        × long "port"
        ⊕ short 'p'
        ⊕ metavar "PORT"
        ⊕ help "port (ignored for connection type 'icmp')"
    <*< cpTtlL .:: optional × option auto
        × long "ttl"
        ⊕ metavar "INT"
        ⊕ help "time to live header value"
    <*< cpTimeoutL . from d2N .:: option auto
        × long "timeout"
        ⊕ short 't'
        ⊕ metavar "FLOAT"
        ⊕ help "connection timeout in seconds"
    <*< cpTraceL .:: switch
        × long "trace"
        ⊕ help "trace routing of the connection"

-- -------------------------------------------------------------------------- --
-- Result

data ConnectionResult = ConnectionResult
    { crResolvedAddr ∷ !SockAddr
    , crSocketAllocationTime ∷ !NominalDiffTime
    , crResolutionTime ∷ !NominalDiffTime
    , crConnectionTime ∷ !NominalDiffTime
    , crTrace ∷ !(Maybe ConnectionFailure)
    }
    deriving (Show, Eq, Typeable)

data ConnectionFailure
    = SocketAllocationFailure T.Text
    | NameResolutionFailure T.Text
    | ConnectionFailure T.Text
    | TimeToLiveExceededFailure T.Text
    | NotReachableFailure NotReachableDetail
    | UnexpectedResponseFailure T.Text
    | InternalFailure T.Text
    | SumFailure [ConnectionFailure]
    deriving (Show, Read, Eq, Ord, Typeable)

instance Monoid ConnectionFailure where
    mempty = SumFailure []
    mappend (SumFailure a) (SumFailure b) = SumFailure (a ⊕ b)
    mappend (SumFailure a) b = SumFailure (a ⊕ [b])
    mappend a (SumFailure b) = SumFailure (a:b)
    mappend a b = SumFailure [a,b]

instance Exception ConnectionFailure

-- | Cf. <http://en.wikipedia.org/wiki/Internet_Control_Message_Protocol#Control_messages>
--
data NotReachableDetail
    = NetworkUnreachable
    -- ^ Network unreachable error.
    | HostUnreachable
    -- ^ Host unreachable error.
    | ProtocolUnreachable
    -- ^ Protocol unreachable error (the designated transport protocol is not supported).
    | PortUnreachable
    -- ^ Port unreachable error (the designated protocol is unable to inform the host of the incoming message).
    | DatagramToBig
    -- ^ The datagram is too big. Packet fragmentation is required but the 'don't fragment' (DF) flag is on.
    | SourceRouteFailed
    -- ^ Source route failed error.
    | DestintationNetworkUnknown
    -- ^ Destination network unknown error.
    | DestintationHostUnknown
    -- ^ Destination host unknown error.
    | SourceHostIsolated
    -- ^ Source host isolated error.
    | DestinationNetworkProhibited
    -- ^ The destination network is administratively prohibited.
    | DestinationHostProhibited
    -- ^ The destination host is administratively prohibited.
    | NetworkUnreachableTOS
    -- ^ The network is unreachable for Type Of Service.
    | HostUnreachableTOS
    -- ^ The host is unreachable for Type Of Service.
    | CommunicationProhibited
    -- ^ Communication administratively prohibited (administrative filtering prevents packet from being forwarded).
    | HostPrecedenceVioloation
    -- ^ Host precedence violation (indicates the requested precedence is not permitted for the combination of host or network and port).
    | PrecedenceCutoff
    -- ^ Precedence cutoff in effect (precedence of datagram is below the level set by the network administrators).
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

-- -------------------------------------------------------------------------- --
-- Connectivity Test

-- TCP
checkConnection ∷ ConnectionParams → EitherT ConnectionFailure IO ConnectionResult

checkConnection params@ConnectionParams{ cpTrace = True } = go mempty (Just 1)
  where
    go e ttl = catchT (run e ttl) $ \e0 → if ttl == cpTtl params
        then left (e ⊕ e0)
        else go (e ⊕ e0) ((+ 1) <$> ttl)

    run e ttl = do
        result ← checkConnection params { cpTtl = ttl , cpTrace = False }
        return $ result { crTrace = Just e }

checkConnection params@ConnectionParams{ cpType = TCP } = do
    -- allocate socket
    (socketAllocationTime, sock) ← tryC SocketAllocationFailure . time $
        getProtocolNumber "tcp" >>= socket AF_INET Stream
    case cpTtl params of
        Just ttl → liftIO $ setSocketOption sock TimeToLive ttl
        Nothing → return ()

    -- Allocate socket
    bracket (return sock) (liftIO . sClose) $ \_ → do

        -- resolve host name
        (resolutionTime, addr) ← tryC NameResolutionFailure . time $ addrAddress . head
            <$> getAddrInfo
                (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
                (Just $ cpHost params)
                (Just . show $ cpPort params)

        -- connect to host
        case addr of
            (SockAddrInet _ _) → timeoutT "connect" ConnectionFailure (cpTimeout params) $ do
                (connectionTime, _) ← tryC ConnectionFailure . time $ connect sock addr
                return ConnectionResult
                    { crResolvedAddr = addr
                    , crSocketAllocationTime = socketAllocationTime
                    , crResolutionTime = resolutionTime
                    , crConnectionTime = connectionTime
                    , crTrace = Nothing
                    }

            a → left . NameResolutionFailure $
                "Address resolution with unexpected result: " ⊕ sshow a

-- UDP
checkConnection params@ConnectionParams{ cpType = UDP } = do

    localAddr ← tryC InternalFailure $ addrAddress . head <$> getAddrInfo
        (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
        Nothing
        (Just "30303")

    -- allocate socket
    (socketAllocationTime, sock) ← tryC SocketAllocationFailure . time $
        getProtocolNumber "udp" >>= socket AF_INET Datagram

    -- Allocate socket
    bracket (return sock) (liftIO . sClose) $ \_ → do

        -- resolve host name
        (resolutionTime, addr) ← tryC NameResolutionFailure . time $ addrAddress . head
            <$> getAddrInfo
                (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
                (Just $ cpHost params)
                (Just . show $ cpPort params)

        -- ping host
        case addr of
            (SockAddrInet _ _) → do
                (connectionTime, result) ← timeT . timeoutT "udp ping" ConnectionFailure (cpTimeout params) $
                    tryC ConnectionFailure $ do
                        bindSocket sock localAddr
                        -- setSocketOption sock ReuseAddr 1
                        case cpTtl params of
                            Just ttl → setSocketOption sock TimeToLive ttl
                            Nothing → return ()
                        void $ sendTo sock "udp ping" addr
                        recvFrom sock 2048
                checkResult result
                return ConnectionResult
                    { crResolvedAddr = addr
                    , crSocketAllocationTime = socketAllocationTime
                    , crResolutionTime = resolutionTime
                    , crConnectionTime = connectionTime
                    , crTrace = Nothing
                    }

            a → left . NameResolutionFailure $
                "Address resolution with unexpected result: " ⊕ sshow a
  where
    checkResult (resp, recAddr) = do
        let (t:c:_) = drop 20 . B.unpack $ resp -- should also look at id
        case (t,c) of
            (0,0) → return ()
            (11,0) → left $ TimeToLiveExceededFailure $ sshow recAddr
            (3,i) → left $ NotReachableFailure $ toEnum (fromIntegral i)
            a → left $ UnexpectedResponseFailure $ "unexpected ICMP ping response from " ⊕ sshow recAddr ⊕ ": " ⊕ sshow a

-- ICMP ping
#if mingw32_HOST_OS == 1
checkConnection params@ConnectionParams{ cpType = ICMP } = do

    -- resolve host name
    (resolutionTime, addr) ← tryC NameResolutionFailure . time $ addrAddress . head
        <$> getAddrInfo
            (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
            (Just $ cpHost params)
            (Just . show $ cpPort params)

    case addr of
        (SockAddrInet _ ipv4) → do
            (connectionTime, result) ← tryC InternalFailure . time $
                icmpEcho (IPv4Addr ipv4) ttl (cpTimeout params) dat
            case result of
                Left e → left . ConnectionFailure $ sshow e ⊕ " (" ⊕ getIcmpEchoErrorMessage e ⊕ ")"
                Right [reply] → case _ierStatus reply of
                    IP_BUF_TOO_SMALL → left $ ConnectionFailure $ sshow reply
                    IP_DEST_NET_UNREACHABLE → left $ NotReachableFailure NetworkUnreachable
                    IP_DEST_HOST_UNREACHABLE → left $ NotReachableFailure HostUnreachable
                    IP_DEST_PROT_UNREACHABLE → left $ NotReachableFailure ProtocolUnreachable
                    IP_DEST_PORT_UNREACHABLE → left $ NotReachableFailure PortUnreachable
                    IP_NO_RESOURCES → left . ConnectionFailure $ sshow reply
                    IP_BAD_OPTION → left . ConnectionFailure $ sshow reply
                    IP_HW_ERROR → left . ConnectionFailure $ sshow reply
                    IP_PACKET_TOO_BIG → left . ConnectionFailure $ sshow reply
                    IP_REQ_TIMED_OUT → left . TimeToLiveExceededFailure $ sshow (_ierIpAddr reply)
                    IP_BAD_REQ → left . ConnectionFailure $ sshow reply
                    IP_BAD_ROUTE → left . ConnectionFailure $ sshow reply
                    IP_TTL_EXPIRED_TRANSIT → left . TimeToLiveExceededFailure $ sshow (_ierIpAddr reply)
                    IP_TTL_EXPIRED_REASSEM → left . ConnectionFailure $ sshow reply
                    IP_PARAM_PROBLEM → left . ConnectionFailure $ sshow reply
                    IP_SOURCE_QUENCH → left . ConnectionFailure $ sshow reply
                    IP_OPTION_TOO_BIG → left . ConnectionFailure $ sshow reply
                    IP_BAD_DESTINATION → left . ConnectionFailure $ sshow reply
                    IP_GENERAL_FAILURE → left . ConnectionFailure $ sshow reply
                    IP_SUCCESS → return ConnectionResult
                        { crResolvedAddr = addr
                        , crSocketAllocationTime = connectionTime - _ierRoundTripTime reply
                        , crResolutionTime = resolutionTime
                        , crConnectionTime = _ierRoundTripTime reply
                        , crTrace = Nothing
                        }
                Right replies → left $ UnexpectedResponseFailure $
                    "To many ICMP replies: " ⊕ sshow replies

        a → left . NameResolutionFailure $
            "Address resolution with unexpected result: " ⊕ sshow a
  where
    ttl = fromMaybe 128 $ cpTtl params
    dat = ""

#else
checkConnection params@ConnectionParams{ cpType = ICMP } = do

    localAddr ← tryC InternalFailure $ addrAddress . head <$> getAddrInfo
        (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
        Nothing
        (Just "30303")

    -- allocate socket
    (socketAllocationTime, sock) ← tryC SocketAllocationFailure . time $
        getProtocolNumber "icmp" >>= socket AF_INET Raw

    -- Allocate socket
    bracket (return sock) (liftIO . sClose) $ \_ → do

        -- resolve host name
        (resolutionTime, addr) ← tryC NameResolutionFailure . time $ addrAddress . head
            <$> getAddrInfo
                (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
                (Just $ cpHost params)
                (Just . show $ cpPort params)

        -- ping host
        case addr of
            (SockAddrInet _ _) → do
                (connectionTime, result) ← timeT . timeoutT "icmp ping" ConnectionFailure (cpTimeout params) $
                    tryC ConnectionFailure $ do
                        bindSocket sock localAddr
                        -- setSocketOption sock ReuseAddr 1
                        setSocketOption sock TimeToLive ttl
                        void $ sendTo sock msg addr
                        recvFrom sock 1024
                checkResult result
                return ConnectionResult
                    { crResolvedAddr = addr
                    , crSocketAllocationTime = socketAllocationTime
                    , crResolutionTime = resolutionTime
                    , crConnectionTime = connectionTime
                    , crTrace = Nothing
                    }

            a → left . NameResolutionFailure $
                "Address resolution with unexpected result: " ⊕ sshow a
  where
    ttl = fromMaybe 128 $ cpTtl params

    msg = B.pack $ map fromIntegral
        [ 8 -- echo request
        , 0
        , div checksum 256
        , mod checksum 256
        , 47
        , 242
        , div ttl 256
        , mod ttl 256
        ]
    checksum = fromIntegral . complement $ (fromIntegral (14322 + ttl) ∷ Word16)

    checkResult (resp, recAddr) = do
        let (t:c:_) = drop 20 . B.unpack $ resp -- should also look at id
        case (t,c) of
            (0,0) → return ()
            (11,0) → left $ TimeToLiveExceededFailure $ sshow recAddr
            (3,i) → left $ NotReachableFailure $ toEnum (fromIntegral i)
            a → left $ UnexpectedResponseFailure $ "unexpected ICMP ping response: " ⊕ sshow a
#endif

