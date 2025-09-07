{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forever, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Language.Haskell.Interpreter
import System.Directory (getModificationTime)
import System.FSNotify
import System.FilePath (takeFileName)
import Data.Time.Clock (UTCTime)

watchFile :: FilePath
watchFile = "Dynamic.hs"

main :: IO ()
main = do
    initialTime <- getModificationTime watchFile
    lastTimeRef <- newIORef initialTime
    withManager $ \mgr -> do
        putStrLn $ "Watching " ++ watchFile ++ " for changes..."
        let whenFile path action =
                when (takeFileName path == watchFile) action
            reloadIfNew = do
                mTime <- try (getModificationTime watchFile) :: IO (Either IOException UTCTime)
                case mTime of
                    Right newTime -> do
                        lastTime <- readIORef lastTimeRef
                        when (newTime > lastTime) $ do
                            writeIORef lastTimeRef newTime
                            reload
                    Left _ -> pure ()
        _ <- watchDir mgr "." (const True) $ \event ->
            case event of
                Added path _ _ -> whenFile path reloadIfNew
                Modified path _ _ -> whenFile path reloadIfNew
                _ -> pure ()
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
