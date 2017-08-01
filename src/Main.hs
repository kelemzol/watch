
{-# LANGUAGE ViewPatterns
           , RecordWildCards
           #-}

module Main where

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
  | Prt String
  deriving (Eq, Show)

main :: IO ()
main = do
    ch <- newChan
    args <- getArgs
    prompt <- newMVar "% "
    case elem "slave" args of
        True -> do
            hSetNewlineMode stdin (NewlineMode LF LF)
            hSetNewlineMode stdout (NewlineMode LF LF)
            hSetNewlineMode stderr (NewlineMode LF LF)
            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            hSetBuffering stderr NoBuffering
            liftIO $ modifyMVarMasked_ prompt (\ _ -> return "")
        False -> return ()
    killPrinter <- startPrinter ch
    runInputTWithPrefs
      defaultPrefs
      Settings { complete = compl, historyFile = Just ".history", autoAddHistory = True }
      (loop prompt (killPrinter, ch) [])
  where
    loop :: MVar String -> P -> DB -> InputT IO ()
    loop prompt p db = do
        promptStr <- liftIO $ readMVar prompt
        minput <- getInputLine promptStr
        case minput of
            Nothing -> return ()
            Just "" -> loop prompt p db
            Just (words -> ["exit"])    -> do
                liftIO (fst p)
                printP p ""
                return ()
            Just (words -> ["watch", wfn]) -> do
                wman <- watch p wfn
                printP p ""
                loop prompt p (DBE{..}:db)
            Just (words -> ["list"]) -> do
                list db
                loop prompt p db
            Just (words -> ["stop", fn]) -> do
                stop p fn db
                loop prompt p (filter (\ DBE{..} -> fn /= wfn) db)
            Just (words -> ["history"]) -> getHistory >>= mapM_ outputStrLn . historyLines >> loop prompt p db
            Just ('e':'c':'h':'o':' ':strs) -> do
                printP p strs
                loop prompt p db
            Just input -> do
                printP p $ "not found command: `" ++ input ++ "`"
                loop prompt p db

compl :: CompletionFunc IO
compl = completeWord (Just '\t') [' '] h
  where
    h "" = return [histC, exitC, wathC, listC, stopC, echoC]
    h ((`isPrefixOf` "history") -> True)    = return [histC]
    h ((`isPrefixOf` "exit") -> True)       = return [exitC]
    h ((`isPrefixOf` "watch") -> True)      = return [wathC]
    h ((`isPrefixOf` "list") -> True)       = return [listC]
    h ((`isPrefixOf` "stop") -> True)       = return [stopC]
    h ((`isPrefixOf` "echo") -> True)       = return [echoC]
    h _ = return []
    histC = Completion "history" "history   - display history" True
    exitC = Completion "exit"    "exit      - exit" True
    wathC = Completion "watch"   "watch dir - starting watch the dir" False
    listC = Completion "list"    "list      - list watched dirs" True
    stopC = Completion "stop"    "stop      - stop whatching" True
    echoC = Completion "echo"    "echo      - echo" True

list :: DB -> InputT IO ()
list = liftIO . putStrLn . unlines . map wfn

watch :: P -> String -> InputT IO WatchManager
watch p fn = liftIO $ do
    man <- startManager
    fn' <- canonicalizePath fn
    watchTree
        man
        fn'
        (const True)
        (writeChan (snd p) . event2PE)
    return man

event2PE :: Event -> PE
event2PE (Added str _) = Add str
event2PE (Modified str _) = Mod str
event2PE (Removed str _) = Rem str

stop :: P -> String -> DB -> InputT IO ()
stop p fn [] = printP p "no whatching this"
stop p fn (DBE{..}:dbo) = if wfn /= fn then stop p fn dbo else liftIO (stopManager wman)

startPrinter :: Chan PE -> IO (IO ())
startPrinter ch = do
    mv <- newMVar []

    t1 <- forkIO $ void $ forever $ do
      e <- readChan ch
      modifyMVarMasked_ mv (\ l -> return (e:l))

    t2 <- forkIO $ void $ forever $ do
      threadDelay (1000*1000)
      modifyMVarMasked_ mv $ \ l -> do
          forM_ (nub l) (hPutStr stdout . (++"\n") . show)
          hFlush stdout
          return []

    return $ do
        killThread t1
        killThread t2


printP :: P -> String -> InputT IO ()
printP (_,ch) = liftIO . writeChan ch . Prt
