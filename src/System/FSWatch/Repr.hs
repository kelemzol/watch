module System.FSWatch.Repr where


import Control.Concurrent (MVar, Chan)

import System.FSNotify (WatchManager)
import System.IO (IO, Handle)
import System.Process (ProcessHandle)

data WatchProcess = WatchProcess
  { wPath :: String
  , wProcessHandle :: ProcessHandle
  , wStdin :: Handle
  , wStdout :: Handle
  , wNotifyMVar :: MVar [PE]
  , wShutdown :: IO ()
  }

type Listener = PE -> IO ()

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
  deriving (Eq, Show, Read)

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
