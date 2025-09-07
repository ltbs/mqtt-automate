{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Language.Haskell.Interpreter
import System.Directory (getModificationTime)
import System.FSNotify
import System.FilePath (takeFileName)

watchFile :: FilePath
watchFile = "Dynamic.hs"

main :: IO ()
main = do
    initialTime <- getModificationTime watchFile
    lastTimeRef <- newIORef initialTime
    withManager $ \mgr -> do
        putStrLn $ "Watching " ++ watchFile ++ " for changes..."
        _ <- watchDir mgr "." (const True) $ \event ->
            when (takeFileName (eventPath event) == watchFile) $ do
                newTime <- getModificationTime watchFile
                lastTime <- readIORef lastTimeRef
                when (newTime > lastTime) $ do
                    writeIORef lastTimeRef newTime
                    reload
        forever $ threadDelay maxBound

reload :: IO ()
reload = do
    putStrLn "Reloading..."
    result <- runInterpreter $ do
        loadModules [watchFile]
        setTopLevelModules ["Dynamic"]
        interpret "greet" (as :: IO ())
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right action -> action
