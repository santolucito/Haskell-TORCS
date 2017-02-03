{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Counter / Incrementer using events and with delay.
-- also uses pure haskell backend, which works on linux

-- sources:
--   https://wiki.haskell.org/Yampa/reactimate
--   incrementer

import Data.IORef
import Data.Time.Clock
import GHC.IO.Handle
import System.IO
import Data.Char
import Data.ByteString (ByteString)

import Control.Monad (unless)
import Network.Socket hiding (sendTo,recvFrom)
import Network.Socket.ByteString (sendTo,recvFrom)
import Control.Exception

import FRP.Yampa
