module Http.Headers
 ( Headers
 , serializeHeaders
 , WithHeaders(..)
 ) where

import Data.List

type Headers = [(String, String)]

serializeHeaders :: Headers -> String
serializeHeaders hs = intercalate "\n" (map fmt hs)
  where
    fmt :: (String, String) -> String
    fmt (k, v) = k ++ ": " ++ v

class WithHeaders a where
  getHeaders :: a -> Headers

  header :: String -> a -> Maybe String
  header name x =
    case [v | (n, v) <- (getHeaders x), n == name] of
      v : _ -> Just v
      _     -> Nothing