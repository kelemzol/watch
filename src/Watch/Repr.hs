module Watch.Repr where

import Data.List

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Console.Haskeline.Completion
import System.Directory
import System.FSNotify
import System.IO
import System.Environment


data Opts = Opts
  { oSlave :: Bool
  , oFixBufferMode :: Int
  , oDelayedBufferMode :: Int
  }

data State = State
  { prompt :: MVar String
  , printFormat :: MVar PrintFormat
  , buffering :: MVar NotifyBuffering
  , mode :: MVar Mode
  }


data DBE = DBE
  { wman :: WatchManager
  , wfn :: String
  }

type DB = [DBE]
type P = (IO (), Chan PE)


data PE
  = Mod String
  | Add String
  | Rem String
  | Prt { fromPrt :: String }
  deriving (Eq, Show)

isPrt :: PE -> Bool
isPrt (Prt _) = True
isPrt _ = False

data PrintFormat
  = MultiRecord
  | SingleRecord
  deriving (Eq, Show)

data Mode
  = CLI
  | SLAVE
  deriving (Eq, Show)

data NotifyBuffering
  = NoNotifyBuffer
  | FixTimeBuffer Int
  | DelayedBuffer Int
  deriving (Eq, Show)
