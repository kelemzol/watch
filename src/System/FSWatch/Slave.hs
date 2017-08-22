

{-# LANGUAGE ViewPatterns
           , RecordWildCards
           #-}

module System.FSWatch.Slave ( createWatchProcess
                            , createWatchProcessWithListener
                            , createWatchProcessWL
                            , watch
                            , stop
                            , getNotifies
                            , waitNotifies
                            ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import System.IO
import System.Process

import System.FSWatch.Repr


createWatchProcess :: (MonadIO m) => String -> Int -> m WatchProcess
createWatchProcess wPath dbi = createWatchProcessWL wPath dbi Nothing

createWatchProcessWithListener :: (MonadIO m) => String -> Int -> Listener -> m WatchProcess
createWatchProcessWithListener wPath dbi listener =createWatchProcessWL wPath dbi (Just listener)


createWatchProcessWL :: (MonadIO m) => String -> Int -> Maybe Listener -> m WatchProcess
createWatchProcessWL wPath dbi listener = liftIO $ do
    (Just wStdin, Just wStdout, _, wProcessHandle)
      <- createProcess (proc wPath ["--slave", "--delayed-buffering", show dbi]) { std_in = CreatePipe, std_out = CreatePipe }
    hSetBuffering wStdin NoBuffering
    hSetBuffering wStdout NoBuffering
    hSetNewlineMode wStdin (NewlineMode LF LF)
    hSetNewlineMode wStdout (NewlineMode LF LF)
    wNotifyMVar <- newEmptyMVar
    wPollerThreadId <- forkIO $ void $ forever $ do
        line <- hGetLine wStdout
        let recs = read line
        case listener of
            (Just lsnr) -> forM_ recs lsnr
            _ -> return ()
        ns <- tryTakeMVar wNotifyMVar
        case ns of
            (Just ns') -> putMVar wNotifyMVar (ns' ++ recs)
            Nothing -> putMVar wNotifyMVar recs
    let wShutdown = do
            killThread wPollerThreadId
            terminateProcess wProcessHandle
    return WatchProcess {..}

watch :: (MonadIO m) => WatchProcess -> FilePath -> m ()
watch (WatchProcess {..}) fn = void $ liftIO $ do
    hPutStrLn wStdin ("watch " ++ fn)

stop :: (MonadIO m) => WatchProcess -> FilePath -> m ()
stop (WatchProcess {..}) fn = void $ liftIO $ do
    hPutStrLn wStdin ("stop " ++ fn)

getNotifies :: WatchProcess -> IO [PE]
getNotifies (WatchProcess {..}) = do
    jpes <- tryTakeMVar wNotifyMVar
    case jpes of
        (Just pes) -> return pes
        _ -> return []

waitNotifies :: WatchProcess -> IO [PE]
waitNotifies (WatchProcess {..}) = do
    takeMVar wNotifyMVar
