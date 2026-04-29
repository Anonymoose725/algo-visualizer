module Main where

import API (app)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = case portEnv >>= readMaybe of
        Just p -> p
        Nothing -> 8080
  putStrLn $ "Server running on port " ++ show port
  run port (corsMiddleware app)

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)

corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = ["GET", "POST", "OPTIONS"],
      corsRequestHeaders = ["Content-Type", "Accept"]
    }
