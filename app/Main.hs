{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Language.Haskell.Interpreter
import System.FSNotify
import System.FilePath (takeFileName)

watchFile :: FilePath
watchFile = "Dynamic.hs"

main :: IO ()
main = withManager $ \mgr -> do
    putStrLn $ "Watching " ++ watchFile ++ " for changes..."
    _ <- watchDir mgr "." (const True) $ \event ->
        when (takeFileName (eventPath event) == watchFile) reload
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
