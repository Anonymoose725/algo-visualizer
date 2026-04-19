{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Algorithms.Sorting (bubbleSort)
import Data.Aeson (ToJSON)
import Network.Wai (Application)
import Servant
import Types (Step (..))

type AlgAPI -- API type
  =
  "sort"
    :> "bubble"
    :> QueryParam "input" String
    :> Get '[JSON] [Step]

server :: Server AlgAPI
server = bubbleHandler

{- A note on handlers:
    - functional algorithms are pure, we want to keep it that way. same input => same output
    - webservers are inherently impure, anything can happen. we need to represent this potential failure in haskell
    - Handler a = IO (Either ServerError a) "do IO action, return either an error or a successful value of type a"
    so Handler [Step] is an action that either fails with an HTTP error or succeeds with a list of steps
    inside a handler we
    - return a result OR
    - throwError err400
    like IO but for HTTP error awareness
-}

bubbleHandler :: Maybe String -> Handler [Step]
bubbleHandler Nothing = throwError err400 -- bad
bubbleHandler (Just inputStr) =
  let nums = parseInts inputStr
   in return (bubbleSort nums)

parseInts :: String -> [Int]
parseInts s = map read (splitOn ',' s)

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  let (word, rest) = break (== delim) str -- ==delim predicate
   in word : case rest of
        [] -> []
        (_ : t) -> splitOn delim t

app :: Application
app = serve (Proxy :: Proxy AlgAPI) server