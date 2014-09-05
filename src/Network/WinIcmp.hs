{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Network.WinIcmp
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
module Network.WinIcmp
( IPv4Addr(..)
, getHostAddress
, IpOptionInformation(..)
, IcmpReplyStatus(..)
, IcmpEchoReply(..)
, getIpErrorMessage
, IcmpEchoError(..)
, getIcmpEchoErrorMessage
, icmpEcho
) where

import Control.Applicative
import Control.Error
import Control.Exception
import Control.Monad

import qualified Data.ByteString as B
import Data.Char (isSpace)
import Data.Monoid
import Data.Monoid.Unicode
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable

import Foreign hiding (void)

import Network.Socket

import System.Win32.Types

import System.IO.Unsafe (unsafePerformIO)

type ULONG = Word32
type PWSTR = LPWSTR
type PDWORD = LPDWORD
type POINTER_32 = Word32

type IP_STATUS = ULONG -- defined in ipexport.h

-- -------------------------------------------------------------------------- --
-- IP Addresses

-- | Marshall to @struct IPAddr.S_addr@ which is of type @u_long@.
--
newtype IPv4Addr = IPv4Addr Word32
    deriving (Read, Eq, Ord, Typeable, Storable)

instance Show IPv4Addr where
    show (IPv4Addr w) = L.intercalate "." $ map (show . byte) [0,1,2,3]
      where
        byte ∷ Int → Int
        byte i = fromIntegral $ shiftR w (i * 8) .&. 0x000000ff

getHostAddress
    ∷ T.Text
    → IO (Either T.Text IPv4Addr)
getHostAddress host = withSocketsDo . runEitherT $ do
    addrInfo ← EitherT $ handle (\(e ∷ IOException) → return . Left . T.pack $ show e) $
       Right <$> getAddrInfo
           (Just (defaultHints { addrFlags = [ AI_PASSIVE ] } ))
           (Just $ T.unpack host)
           Nothing
    case addrAddress <$> addrInfo of
        [] → left "getAddrInfo: empty result list"
        SockAddrInet _ ipv4 : _ → right $ IPv4Addr ipv4
        x → left $ "getAddrInfo: unexpected result: " ⊕ T.pack (show x)

-- -------------------------------------------------------------------------- --
-- | IpOptionInformation
--
-- > typedef struct _IP_OPTION_INFORMATION32 {
-- >   UCHAR              Ttl;
-- >   UCHAR              Tos;
-- >   UCHAR              Flags;       // In IPv4, this is the Flags field in the IPv4 header.
-- >   UCHAR              OptionsSize; // The size, in bytes, of IP options data.
-- >   UCHAR * POINTER_32 OptionsData; // A pointer to options data.
-- > } IP_OPTION_INFORMATION32, *PIP_OPTION_INFORMATION32;
--
data C_IpOptionInformation = C_IpOptionInformation
    { _cioiTtl ∷ !UCHAR
    , _cioiTos ∷ !UCHAR
    , _cioiFlags ∷ !UCHAR
    , _cioiOptionsSize ∷ !UCHAR
    , _cioiOptionsData ∷ !(Ptr UCHAR)
    }
    deriving (Show, Eq, Ord, Typeable)

instance Storable C_IpOptionInformation where
    sizeOf = const 8
    alignment = sizeOf
    peek p = C_IpOptionInformation
        <$> peekByteOff p 0
        <*> peekByteOff p 1
        <*> peekByteOff p 2
        <*> peekByteOff p 3
        <*> (wordPtrToPtr . fromIntegral <$> (peekByteOff p 4 ∷ IO POINTER_32))
    poke p C_IpOptionInformation{..} = do
        pokeByteOff p 0 _cioiTtl
        pokeByteOff p 1 _cioiTos
        pokeByteOff p 2 _cioiFlags
        pokeByteOff p 3 _cioiOptionsSize
        pokeByteOff p 4 (fromIntegral $ ptrToWordPtr _cioiOptionsData ∷ POINTER_32)

data IpOptionInformation = IpOptionInformation
    { _ioiTtl ∷ !Word8
    , _ioiTos ∷ !Word8
    , _ioiFlags ∷ !Word8
    , _ioiOptionsData ∷ !B.ByteString
    }
    deriving (Show, Eq, Ord, Typeable)

withCIpOptionInformation
    ∷ IpOptionInformation
    → (Ptr C_IpOptionInformation → IO a)
    → IO a
withCIpOptionInformation ioi action =
    B.useAsCStringLen (_ioiOptionsData ioi) $ \(optDataPtr, size) →
          with C_IpOptionInformation
            { _cioiTtl = fromIntegral $ _ioiTtl ioi
            , _cioiTos = fromIntegral $ _ioiTos ioi
            , _cioiFlags = fromIntegral $ _ioiFlags ioi
            , _cioiOptionsSize = fromIntegral size
            , _cioiOptionsData = castPtr optDataPtr
            }
            action

fromCIpOptionInformation
    ∷ C_IpOptionInformation
    → IO IpOptionInformation
fromCIpOptionInformation cioi = do
    dat ← B.packCStringLen (castPtr (_cioiOptionsData cioi), fromIntegral (_cioiOptionsSize cioi))
    return IpOptionInformation
        { _ioiTtl = fromIntegral $ _cioiTtl cioi
        , _ioiTos = fromIntegral $ _cioiTos cioi
        , _ioiFlags = fromIntegral $ _cioiFlags cioi
        , _ioiOptionsData = dat
        }

defaultIpOptionInformation ∷ IpOptionInformation
defaultIpOptionInformation = IpOptionInformation
    { _ioiTtl = 64
    , _ioiTos = 0
    , _ioiFlags = 0
    , _ioiOptionsData = ""
    }

-- -------------------------------------------------------------------------- --
-- ICMP Echo Reply

data IcmpReplyStatus
    = IP_SUCCESS -- ^ The status was success.
    | IP_BUF_TOO_SMALL -- ^ The reply buffer was too small.
    | IP_DEST_NET_UNREACHABLE -- ^ The destination network was unreachable.
    | IP_DEST_HOST_UNREACHABLE -- ^ The destination host was unreachable.
    | IP_DEST_PROT_UNREACHABLE -- ^ The destination protocol was unreachable.
    | IP_DEST_PORT_UNREACHABLE -- ^ The destination port was unreachable.
    | IP_NO_RESOURCES -- ^ Insufficient IP resources were available.
    | IP_BAD_OPTION -- ^ A bad IP option was specified.
    | IP_HW_ERROR -- ^ A hardware error occurred.
    | IP_PACKET_TOO_BIG -- ^ The packet was too big.
    | IP_REQ_TIMED_OUT -- ^ The request timed out.
    | IP_BAD_REQ -- ^ A bad request.
    | IP_BAD_ROUTE -- ^ A bad route.
    | IP_TTL_EXPIRED_TRANSIT -- ^ The time to live (TTL) expired in transit.
    | IP_TTL_EXPIRED_REASSEM -- ^ The time to live expired during fragment reassembly.
    | IP_PARAM_PROBLEM -- ^ A parameter problem.
    | IP_SOURCE_QUENCH -- ^ Datagrams are arriving too fast to be processed and datagrams may have been discarded.
    | IP_OPTION_TOO_BIG -- ^ An IP option was too big.
    | IP_BAD_DESTINATION -- ^ A bad destination.
    | IP_GENERAL_FAILURE -- ^ A general failure. This error can be returned for some malformed ICMP packets.
    deriving (Show, Read, Eq, Ord, Typeable)

instance Enum IcmpReplyStatus where
    toEnum 0 = IP_SUCCESS
    toEnum 11001 = IP_BUF_TOO_SMALL
    toEnum 11002 = IP_DEST_NET_UNREACHABLE
    toEnum 11003 = IP_DEST_HOST_UNREACHABLE
    toEnum 11004 = IP_DEST_PROT_UNREACHABLE
    toEnum 11005 = IP_DEST_PORT_UNREACHABLE
    toEnum 11006 = IP_NO_RESOURCES
    toEnum 11007 = IP_BAD_OPTION
    toEnum 11008 = IP_HW_ERROR
    toEnum 11009 = IP_PACKET_TOO_BIG
    toEnum 11010 = IP_REQ_TIMED_OUT
    toEnum 11011 = IP_BAD_REQ
    toEnum 11012 = IP_BAD_ROUTE
    toEnum 11013 = IP_TTL_EXPIRED_TRANSIT
    toEnum 11014 = IP_TTL_EXPIRED_REASSEM
    toEnum 11015 = IP_PARAM_PROBLEM
    toEnum 11016 = IP_SOURCE_QUENCH
    toEnum 11017 = IP_OPTION_TOO_BIG
    toEnum 11018 = IP_BAD_DESTINATION
    toEnum 11050 = IP_GENERAL_FAILURE
    toEnum x = error $ "Prelude.toEnum: unsupported IcmpReplyStatus value " ⊕ show x

    fromEnum IP_SUCCESS = 0
    fromEnum IP_BUF_TOO_SMALL = 11001
    fromEnum IP_DEST_NET_UNREACHABLE = 11002
    fromEnum IP_DEST_HOST_UNREACHABLE = 11003
    fromEnum IP_DEST_PROT_UNREACHABLE = 11004
    fromEnum IP_DEST_PORT_UNREACHABLE = 11005
    fromEnum IP_NO_RESOURCES = 11006
    fromEnum IP_BAD_OPTION = 11007
    fromEnum IP_HW_ERROR = 11008
    fromEnum IP_PACKET_TOO_BIG = 11009
    fromEnum IP_REQ_TIMED_OUT = 11010
    fromEnum IP_BAD_REQ = 11011
    fromEnum IP_BAD_ROUTE = 11012
    fromEnum IP_TTL_EXPIRED_TRANSIT = 11013
    fromEnum IP_TTL_EXPIRED_REASSEM = 11014
    fromEnum IP_PARAM_PROBLEM = 11015
    fromEnum IP_SOURCE_QUENCH = 11016
    fromEnum IP_OPTION_TOO_BIG = 11017
    fromEnum IP_BAD_DESTINATION = 11018
    fromEnum IP_GENERAL_FAILURE = 11050

instance Storable IcmpReplyStatus where
    sizeOf = const $ sizeOf (0 ∷ ULONG)
    alignment = sizeOf
    peek p = toEnum . fromIntegral <$> peek (castPtr p ∷ Ptr Word32)
    poke p o = poke (castPtr p ∷ Ptr Word32) (fromIntegral $ fromEnum o)

-- | C ICMP Echo Reply
--
-- > typedef struct icmp_echo_reply {
-- >   IPAddr                       Address;
-- >   ULONG                        Status;
-- >   ULONG                        RoundTripTime;
-- >   USHORT                       DataSize;
-- >   USHORT                       Reserved;
-- >   PVOID                        Data;
-- >   struct ip_option_information  Options;
-- > } ICMP_ECHO_REPLY, *PICMP_ECHO_REPLY;
--
data C_IcmpEchoReply = C_IcmpEchoReply
    { _cierIpAddr ∷ !IPv4Addr
    , _cierStatus ∷ !IcmpReplyStatus
    , _cierRoundTripTime ∷ !ULONG -- ^ in milliseconds
    , _cierDataSize ∷ !USHORT
    , _cierReserved ∷ !USHORT
    , _cierData ∷ !(Ptr ())
    , _cierIpOptionInformation ∷ !C_IpOptionInformation
    }
    deriving (Show, Eq, Ord, Typeable)

instance Storable C_IcmpEchoReply where
    sizeOf = const 32
    alignment = sizeOf
    peek p = C_IcmpEchoReply
        <$> peekByteOff p 0 -- Word32 (4 bytes)
        <*> peekByteOff p 4 -- ULONG (4 bytes)
        <*> peekByteOff p 8 -- ULONG (4 bytes)
        <*> peekByteOff p 12 -- USHORT (2 bytes)
        <*> peekByteOff p 14 -- USHORT (2 bytes)
        <*> (wordPtrToPtr . fromIntegral <$> (peekByteOff p 16 ∷ IO POINTER_32))-- LPVOID (8 bytes) -- FIXME does this depend on the platform?
        <*> peekByteOff p 24 -- IpOptionInformation
    poke p C_IcmpEchoReply{..} = do
        pokeByteOff p 0 _cierIpAddr
        pokeByteOff p 4 _cierStatus
        pokeByteOff p 8 _cierRoundTripTime
        pokeByteOff p 12 _cierDataSize
        pokeByteOff p 14 _cierReserved
        pokeByteOff p 16 (fromIntegral $ ptrToWordPtr _cierData ∷ POINTER_32)
        pokeByteOff p 24 _cierIpOptionInformation

-- | ICMP Echo Reply
--
data IcmpEchoReply = IcmpEchoReply
    { _ierIpAddr ∷ !IPv4Addr
    , _ierStatus ∷ !IcmpReplyStatus
    , _ierRoundTripTime ∷ !NominalDiffTime
    , _ierData ∷ !B.ByteString
    , _ierIpOptionInformation ∷ !IpOptionInformation
    }
    deriving (Show, Eq, Ord, Typeable)

fromCIcmpEchoReply
    ∷ C_IcmpEchoReply
    → IO IcmpEchoReply
fromCIcmpEchoReply cier = do
    dat ← B.packCStringLen (castPtr (_cierData cier), fromIntegral (_cierDataSize cier))
    opt ← fromCIpOptionInformation $ _cierIpOptionInformation cier
    return IcmpEchoReply
        { _ierIpAddr = _cierIpAddr cier
        , _ierStatus = _cierStatus cier
        , _ierRoundTripTime = realToFrac (_cierRoundTripTime cier) / 1000
        , _ierData = dat
        , _ierIpOptionInformation = opt
        }

-- -------------------------------------------------------------------------- --
-- IP Error String

foreign import ccall "Iphlpapi.h GetIpErrorString" c_getIpErrorString
    ∷ IP_STATUS -- ^ Error code
    → PWSTR -- ^ Buffer
    → PDWORD -- ^ Buffer size (in chars including trailing '\0')
    → IO DWORD -- ^ NO_ERROR on success

getIpErrorMessage
    ∷ IcmpReplyStatus
    → T.Text
getIpErrorMessage status = unsafePerformIO $ run 4096
  where
    run ∷ DWORD → IO T.Text
    run n =
        allocaBytes (fromIntegral n) $ \ptr → do
            r ← with n $ \nC →
                c_getIpErrorString (fromIntegral $ fromEnum status) ptr nC
            case r of
                0 → T.pack <$> peekTString ptr
                n_ → run (n_ + 1)

-- -------------------------------------------------------------------------- --

-- | <http://msdn.microsoft.com/en-us/library/windows/desktop/aa366050(v=vs.85).aspx>
--
data IcmpEchoError
    = ERROR_INSUFFICIENT_BUFFER
    -- ^ The data area passed to a system call is too small. This error is returned
    -- if the ReplySize parameter indicates that the buffer pointed to by the
    -- ReplyBuffer parameter is too small.

    | ERROR_INVALID_PARAMETER
    -- ^ An invalid parameter was passed to the function. This error is returned
    -- if the IcmpHandle parameter contains an invalid handle. This error can
    -- also be returned if the ReplySize parameter specifies a value less than
    -- the size of an ICMP_ECHO_REPLY or ICMP_ECHO_REPLY32 structure.

    | ERROR_NOT_ENOUGH_MEMORY
    -- ^ Not enough memory is available to complete the operation.

    | ERROR_NOT_SUPPORTED
    -- ^ The request is not supported. This error is returned if no IPv4 stack
    -- is on the local computer.

    | ERROR_IP_BUF_TOO_SMALL
    -- ^ The size of the ReplyBuffer specified in the ReplySize parameter was too small.

    | ERROR_IP_REQ_TIMED_OUT
    -- ^ The request timed out. This is not documented but derived from
    -- observation on a windows 8 system.

    | ERROR_OTHER Int
    -- ^ Use FormatMessage to obtain the message string for the returned error.
    deriving (Show, Read, Eq, Ord, Typeable)

instance Enum IcmpEchoError where
    fromEnum ERROR_INSUFFICIENT_BUFFER = 122
    fromEnum ERROR_INVALID_PARAMETER = 87
    fromEnum ERROR_NOT_ENOUGH_MEMORY = 8
    fromEnum ERROR_NOT_SUPPORTED = 50
    fromEnum ERROR_IP_BUF_TOO_SMALL = 11001
    fromEnum ERROR_IP_REQ_TIMED_OUT = 11010
    fromEnum (ERROR_OTHER i) = i

    toEnum 122 = ERROR_INSUFFICIENT_BUFFER
    toEnum 87 = ERROR_INVALID_PARAMETER
    toEnum 8 = ERROR_NOT_ENOUGH_MEMORY
    toEnum 50 = ERROR_NOT_SUPPORTED
    toEnum 11010 = ERROR_IP_REQ_TIMED_OUT
    toEnum 11001 = ERROR_IP_BUF_TOO_SMALL
    toEnum x = ERROR_OTHER x

foreign import ccall unsafe "windows.h GetLastError" c_getLastError
    ∷ IO DWORD

-- | Wrapper around @GetLastError@ from @Windows.h@.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx>
--
getIcmpEchoError ∷ IO IcmpEchoError
getIcmpEchoError = toEnum . fromIntegral <$> c_getLastError

type FormatMessageFlags = DWORD
format_message_allocate_buffer, format_message_from_system, format_message_ignore_inserts ∷ FormatMessageFlags
format_message_allocate_buffer = 0x00000100
format_message_from_system = 0x00001000
format_message_ignore_inserts = 0x00000200

--
-- > DWORD WINAPI FormatMessage(
-- >   _In_      DWORD dwFlags,
-- >   _In_opt_  LPCVOID lpSource,
-- >   _In_      DWORD dwMessageId,
-- >   _In_      DWORD dwLanguageId,
-- >   _Out_     LPTSTR lpBuffer,
-- >   _In_      DWORD nSize,
-- >   _In_opt_  va_list *Arguments
-- > );
--
foreign import ccall unsafe "WinBase.h FormatMessageW" c_formatMessage
    ∷ FormatMessageFlags
    → LPVOID -- ^ source (set to NULL)
    → DWORD -- ^ message id / error code
    → DWORD -- ^ language id (a value of 0 picks a reasonable default value)
    → LPTSTR -- ^ buffer pointer
    → DWORD -- ^ size (set to 0 when buffer is allocated by system)
    → Ptr () -- ^ va_list arguments (set to NULL)
    → IO DWORD -- ^ 0 in case of failure, otherwise length of returned message

-- | A wrapper arround @FormatMessageW@ from @Windows.hs@.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms679351(v=vs.85).aspx>
--
-- It seems that the error messages are not allways correct and sometimes
-- even completely missleading. The reason seems to be in overloaded (partially
-- overlapping) use of error codes in windows. In particular @IcmpSendEcho@
-- may return @11010@ for @IP_REQ_TIMED_OUT@ while this function returns
-- @Error due to lack of resources.@ for @WSA_QOS_ADMISSION_FAILURE@ from
-- @WinError.h@.
--
getIcmpEchoErrorMessage
    ∷ IcmpEchoError
    → T.Text
getIcmpEchoErrorMessage errCode = unsafePerformIO $
    bracket allocCMsg deallocCMsg $ \case
        Nothing → return $ "Error " ⊕ T.pack (show errCode)
        -- One more Windows stupidity: trailing linebreaks on error messages.
        Just cmsg → T.dropAround isSpace . T.pack <$> peekTString cmsg
  where
    errNumber = fromIntegral $ fromEnum errCode

    deallocCMsg ∷ Maybe (Ptr a) → IO ()
    deallocCMsg = maybe (return ()) (void . c_localFree . castPtr)

    allocCMsg ∷ IO (Maybe LPTSTR)
    allocCMsg =
        alloca $ \cmsgPtr → do
            poke cmsgPtr nullPtr
            c_formatMessage flags nullPtr errNumber 0 (castPtr cmsgPtr) 0 nullPtr >>= \case
                0 → return Nothing
                _ → do
                    cmsg ← peek cmsgPtr
                    return $ if cmsg == nullPtr
                        then Nothing
                        else Just cmsg

    flags = format_message_allocate_buffer
        .|. format_message_from_system
        .|. format_message_ignore_inserts

foreign import ccall unsafe "windows.h LocalFree" c_localFree
    ∷ Ptr a
    → IO (Ptr a)

-- -------------------------------------------------------------------------- --

foreign import ccall "Icmpapi.h IcmpSendEcho" c_icmpSendEcho
    ∷ HANDLE
    → IPv4Addr -- destination
    → LPVOID -- request data buffer, @LPVOID@
    → WORD -- request size @WORD@
    → Ptr C_IpOptionInformation -- @PIP_OPTION_INFORMATION@
    → LPVOID -- reply data buffer, @LPVOID@
    → DWORD -- reply buffer size @DWORD@
    → DWORD -- timeout in milliseconds @DWORD@
    → IO DWORD -- number of ICMP_ECHO_REPLY or ICMP_ECHO_REPLY32 structures stored in the ReplyBuffer. @DWORD@

foreign import ccall "Icmpapi.h IcmpCreateFile" c_icmpCreateFile
    ∷ IO HANDLE

foreign import ccall "Icmpapi.h IcmpCloseHandle" c_icmpCloseHandle
    ∷ HANDLE
    → IO BOOL

withIcmpHandle
    ∷ (HANDLE → IO a)
    → IO a
withIcmpHandle = bracket c_icmpCreateFile c_icmpCloseHandle

-- | A high-level API for @IcmpSendEcho@ from @Icmpapi.h@.
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/aa366050(v=vs.85).aspx>
--
icmpEcho
    ∷ IPv4Addr -- ^ destination
    → Int -- ^ TTL
    → NominalDiffTime -- ^ timeout in milliseconds
    → B.ByteString
    → IO (Either IcmpEchoError [IcmpEchoReply])
icmpEcho dest ttl timeout dat = withSocketsDo $ withIcmpHandle $ \h →
    B.useAsCStringLen dat $ \(cstr, len) →
    withCIpOptionInformation opt $ \optPtr →
    allocaBytes 4096 $ \bufPtr → do
        let reqPtr = castPtr cstr
        let repPtr = castPtr bufPtr
        n ← c_icmpSendEcho h dest reqPtr (fromIntegral len) optPtr repPtr 4096 (round timeout * 1000)
        case n of
            0 → do
                e ← getIcmpEchoError
                putStrLn $ "error: " ⊕ show e ⊕ " (" ⊕ T.unpack (getIcmpEchoErrorMessage e) ⊕ ")"
                return $ Left e
            _ → do
                r ← peekArray (fromIntegral n) bufPtr
                Right <$> mapM fromCIcmpEchoReply r
  where
    opt = defaultIpOptionInformation
        { _ioiTtl = fromIntegral ttl
        }
