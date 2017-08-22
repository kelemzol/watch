
{-# LANGUAGE ViewPatterns
           , RecordWildCards
           #-}

module System.FSWatch (watchMain, getOpts) where

import Data.List
import Data.Semigroup ((<>))

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Options.Applicative hiding (defaultPrefs)

import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Directory
import System.FSNotify
import System.IO

import System.FSWatch.Repr

optParser :: Parser Opts
optParser = Opts
    <$> switch
        ( long "slave"
       <> help "cli - normal mode; slave - no buffering on std in/out, no prompt, one line records")
    <*> option auto
        ( long "fix-buffering"
       <> metavar "NUMBER"
       <> help "fix time loop; NUMBER in ms"
       <> showDefault
       <> value 0)
    <*> option auto
        ( long "delayed-buffering"
       <> metavar "NUMBER"
       <> help "delayed bufferint from last; NUMBER in ms"
       <> showDefault
       <> value 0)

getOpts :: IO Opts
getOpts = execParser opts
  where
    opts = info (optParser <**> helper)
      ( fullDesc
     <> progDesc "File watching tool"
     <> header "[header]" )

watchMain :: IO ()
watchMain = do
    opts <- getOpts
    ch <- newChan
    prompt <- newMVar "% "
    printFormat <- newMVar MultiRecord
    buffering <- newMVar $ case (oFixBufferMode opts, oDelayedBufferMode opts) of
      (0,0) -> NoNotifyBuffer
      (0,i) -> DelayedBuffer i
      (i,_) -> FixTimeBuffer i
    mode <- newMVar CLI
    case oSlave opts of
        True -> do
            hSetNewlineMode stdin (NewlineMode LF LF)
            hSetNewlineMode stdout (NewlineMode LF LF)
            hSetNewlineMode stderr (NewlineMode LF LF)
            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            hSetBuffering stderr NoBuffering
            liftIO $ modifyMVarMasked_ prompt (\ _ -> return "")
            liftIO $ modifyMVarMasked_ printFormat (\ _ -> return SingleRecord)
            liftIO $ modifyMVarMasked_ mode (\ _ -> return SLAVE)
        False -> return ()
    killPrinter <- startPrinter (State {..}) printFormat ch
    runInputTWithPrefs
      defaultPrefs
      Settings { complete = compl, historyFile = Nothing, autoAddHistory = False }
      (loop (State {..}) (killPrinter, ch) [])
  where
    loop :: State -> P -> DB -> InputT IO ()
    loop state@(State {..}) p db = do
        promptStr <- liftIO $ readMVar prompt
        minput <- getInputLine promptStr
        case minput of
            Nothing -> return ()
            Just "" -> loop state p db
            Just (words -> ["exit"])    -> do
                liftIO (fst p)
                printP p ""
                return ()
            Just (words -> ["watch", wfn]) -> do
                wman <- watch p wfn
                printP p ""
                loop state p (DBE{..}:db)
            Just (words -> ["list"]) -> do
                list db
                loop state p db
            Just (words -> ["stop", fn]) -> do
                stop p fn db
                loop state p (filter (\ DBE{..} -> fn /= wfn) db)
            Just (words -> ["history"]) -> getHistory >>= mapM_ outputStrLn . historyLines >> loop state p db
            Just ('e':'c':'h':'o':' ':strs) -> do
                printP p strs
                loop state p db
            Just (words -> ["buffering", "fix", parseInt -> (Just i)]) -> do
                liftIO $ modifyMVarMasked_ buffering (\_-> return (FixTimeBuffer i))
                loop state p db
            Just (words -> ["buffering", "delayed", parseInt -> (Just i)]) -> do
                liftIO $ modifyMVarMasked_ buffering (\_-> return (DelayedBuffer i))
                loop state p db
            Just (words -> ["no", "buffering"]) -> do
                liftIO $ modifyMVarMasked_ buffering (\_-> return NoNotifyBuffer)
                loop state p db
            Just input -> do
                printP p $ "not found command: `" ++ input ++ "`"
                loop state p db

parseInt :: String -> Maybe Int
parseInt (reads -> [(i, _)]) = Just i
parseInt _ = Nothing

compl :: CompletionFunc IO
compl = {- compl' -} completeWord Nothing {- (Just '\t') -} [] h
  where
    h "" = return [histC, exitC, wathC, listC, stopC, echoC, buffC, bufdC, nobuffC]
    h ((`isPrefixOf` "history") -> True)    = return [histC]
    h ((`isPrefixOf` "exit") -> True)       = return [exitC]
    h ((`isPrefixOf` "watch") -> True)      = return [wathC]
    h ((`isPrefixOf` "list") -> True)       = return [listC]
    h ((`isPrefixOf` "stop") -> True)       = return [stopC]
    h ((`isPrefixOf` "echo") -> True)       = return [echoC]
    h (words -> [(`isPrefixOf` "buffering") -> True])                = return [buffC, bufdC]
    h (words -> ["buffering", (`isPrefixOf` "fix") -> True])         = return [fixC]
    h (words -> ["buffering", (`isPrefixOf` "delayed") -> True])     = return [delayedC]
    h (words -> [(`isPrefixOf` "no") -> True])                       = return [nobuffC]
    h (words -> ["no", (`isPrefixOf` "buffering") -> True])          = return [nobuff_C]
    h _ = return []
    histC =    Completion "history"            "history                    - display history" False
    exitC =    Completion "exit"               "exit                       - exit" False
    wathC =    Completion "watch"              "watch dir                  - starting watch the dir" True
    listC =    Completion "list"               "list                       - list watched dirs" False
    stopC =    Completion "stop"               "stop                       - stop whatching" True
    echoC =    Completion "echo"               "echo                       - echo" True
    buffC =    Completion "buffering "         "buffering fix [number]     - notify buffering in fix time (ms)" True
    bufdC =    Completion "buffering "         "buffering delayed [number] - notify buffering in delayed time from last notify (ms)" True
    nobuffC =  Completion "no"                 "no buffering               - no notify buffering" True
    nobuff_C = Completion "no buffering"       "no buffering               - no notify buffering" False
    fixC =     Completion "buffering fix"      "buffering fix [number]     - notify buffering in fix time (ms)" True
    delayedC = Completion "buffering delayed"  "buffering delayed [number] - notify buffering in delayed time from last notify (ms)" True


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

startPrinter :: State -> MVar PrintFormat -> Chan PE -> IO (IO ())
startPrinter (State {..}) pfm ch = do
    mv <- newMVar []
    noBufferClock <- newEmptyMVar
    fixBufferClock <- newEmptyMVar
    delayedBufferClock <- newEmptyMVar
    bf <- readMVar buffering
    lastBufferMode <- newMVar bf

    t1 <- forkIO $ void $ forever $ do
        e <- readChan ch
        tryTakeMVar noBufferClock
        tryTakeMVar fixBufferClock
        tryTakeMVar delayedBufferClock
        bf <- readMVar buffering
        case bf of
            NoNotifyBuffer -> putMVar noBufferClock ()
            (FixTimeBuffer _) -> putMVar fixBufferClock ()
            (DelayedBuffer _) -> putMVar delayedBufferClock ()
        modifyMVarMasked_ mv (\ l -> return (e:l))

    t2 <- forkIO $ void $ forever $ do
        readMVar fixBufferClock
        bf <- readMVar buffering
        case bf of
            (FixTimeBuffer i) -> do
                threadDelay (1000*i)
                printerOut (State {..}) pfm mv
            _ -> return ()

    t3 <- forkIO $ void $ forever $ do
        takeMVar noBufferClock
        bf <- readMVar buffering
        case bf of
            (NoNotifyBuffer) -> do
                printerOut (State {..}) pfm mv
            _ -> return ()

    t4 <- forkIO $ void $ forever $ do
        readMVar delayedBufferClock
        bf <- readMVar buffering
        case bf of
            (DelayedBuffer i) -> do
                printerOut (State {..}) pfm mv
                elems <- readMVar mv
                let loop elems = do
                      threadDelay (1000*i)
                      elems' <- readMVar mv
                      if elems == elems'
                        then printerOut (State {..}) pfm mv
                        else loop elems'
                loop elems
            _ -> return ()

    return $ do
        killThread t1
        killThread t2
        killThread t3
        killThread t4

printerOut (State {..}) pfm mv = do
    pf <- readMVar pfm
    m <- readMVar mode
    modifyMVarMasked_ mv $ \ l -> do
        let prts = filter isPrt l
            signals = nub (filter (not . isPrt) l)
        case m of
            CLI -> forM_ prts (hPutStr stdout . (++"\n") . fromPrt)
            SLAVE -> return ()
        case pf of
            MultiRecord  -> forM_ signals (hPutStr stdout . (++"\n") . show)
            SingleRecord -> if null signals then return () else hPutStr stdout ((show signals) ++"\n")
        hFlush stdout
        return []


printP :: P -> String -> InputT IO ()
printP (_,ch) = liftIO . writeChan ch . Prt
