module Main where

import API (app)
-- run :: Port -> Application -> IO () listens for HTTP reqs from port number
-- for listening to frontend on port 3000
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors

main :: IO ()
main = do
  putStrLn "Server running on port 8080"
  run 8080 (corsMiddleware app)

corsMiddleware :: Middleware -- wraps app in middleware, every request passes through before reaching Servant
corsMiddleware = cors (const $ Just corsPolicy) -- admits all requests via policy Just corsPolicy

corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy -- default policy, customized with record below
    { corsOrigins = Nothing, -- allow requests from any origin (for development!)
      corsMethods = ["GET", "POST", "OPTIONS"], -- which http methods we need to allow
      corsRequestHeaders = ["Content-Type", "Accept"] -- which headers to allow, content type is needed for JSON
    }