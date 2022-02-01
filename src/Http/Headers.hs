module Http.Headers
  ( Headers
  , serializeHeaders
  ) where

import           Data.List

type Headers = [(String, String)]

serializeHeaders :: Headers -> String
serializeHeaders hs = intercalate "\n" (map fmt hs)
  where
    fmt :: (String, String) -> String
    fmt (k, v) = k ++ ": " ++ v
