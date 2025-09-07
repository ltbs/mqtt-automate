{-# LANGUAGE OverloadedStrings #-}

-- | Dynamic module defining MQTT subscriptions and handlers.
module Dynamic where

-- | Topic filters to subscribe to. Each entry is an MQTT topic filter.
subscriptions :: [String]
subscriptions = ["sensors/temperature", "alerts/#"]

-- | Reaction to an incoming MQTT message.
--   The first argument is the topic on which the message was received,
--   the second argument is the textual payload.
onMessage :: String -> String -> IO ()
onMessage topic payload =
    putStrLn $ "Received on " ++ topic ++ ": " ++ payload
