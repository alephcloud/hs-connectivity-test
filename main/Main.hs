{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@alephcloud.com>
-- Stability: experimental
--
module Main
( main
) where

import Control.Error

import Data.String
import qualified Data.Text.IO as T

import Network.ConnectivityTest
import Network.Socket

import PkgInfo

import System.Exit
import System.IO

import Configuration.Utils

-- -------------------------------------------------------------------------- --
-- Misc Utils

sshow ∷ (Show a, IsString b) ⇒ a → b
sshow = fromString . show

-- -------------------------------------------------------------------------- --
-- Main

mainInfo ∷ ProgramInfo ConnectionParams
mainInfo = programInfo "Connection Test" pConnectionParams defaultConnectionParams

main ∷ IO ()
main = withSocketsDo $ runWithPkgInfoConfiguration mainInfo pkgInfo $ \params →
    runEitherT (checkConnection params) >>= \case
        Left e → do
            T.hPutStrLn stderr $ sshow e
            exitFailure
        Right r → do
            T.hPutStrLn stdout $ sshow r
            exitSuccess

