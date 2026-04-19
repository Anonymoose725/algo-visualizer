module Main where

import API (app)
import Network.Wai.Handler.Warp (run) -- run :: Port -> Application -> IO () listens for HTTP reqs from port number

main :: IO ()
main = do
  putStrLn "Server running on port 8080"
  run 8080 app
