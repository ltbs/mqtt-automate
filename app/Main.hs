{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forever, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Language.Haskell.Interpreter
import System.Directory (getModificationTime)
import System.FSNotify
import System.FilePath (takeFileName)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.MQTT.Client (MQTTClient, mqttConfig, connectURI, subscribe, MessageCallback(..))
import Network.MQTT.Types (QoS(..))
import Network.URI (parseURI)

watchFile :: FilePath
watchFile = "Dynamic.hs"

-- | Reload the dynamic module and update subscriptions and handlers.
reload :: MQTTClient -> IORef (String -> String -> IO ()) -> IO ()
reload client handlerRef = do
    putStrLn "Reloading..."
    result <- runInterpreter $ do
        loadModules [watchFile]
        setTopLevelModules ["Dynamic"]
        subs <- interpret "subscriptions" (as :: [String])
        handler <- interpret "onMessage" (as :: String -> String -> IO ())
        return (subs, handler)
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (subs, handler) -> do
            writeIORef handlerRef handler
            -- subscribe to new topics
            _ <- subscribe client $ zip (T.pack <$> subs) (repeat QoS0)
            return ()

main :: IO ()
main = do
    initialTime <- getModificationTime watchFile
    lastTimeRef <- newIORef initialTime
    handlerRef <- newIORef (\_ _ -> pure ())

    -- set up MQTT client with dynamic callback
    let cb _ topic payload = do
            h <- readIORef handlerRef
            h (T.unpack topic) (BL8.unpack payload)
    let cfg = mqttConfig { _msgCB = SimpleCallback cb }
    let Just uri = parseURI "mqtt://test.mosquitto.org"
    client <- connectURI cfg uri

    -- initial load of dynamic module
    reload client handlerRef

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
                            reload client handlerRef
                    Left _ -> pure ()
        _ <- watchDir mgr "." (const True) $ \event ->
            case event of
                Added path _ _ -> whenFile path reloadIfNew
                Modified path _ _ -> whenFile path reloadIfNew
                _ -> pure ()
        forever $ threadDelay maxBound
