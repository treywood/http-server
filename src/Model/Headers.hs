module Model.Headers
 ( Headers
 , serialize
 ) where

import Data.List

type Headers = [(String, String)]

serialize :: Headers -> String
serialize hs = intercalate "\n" (map fmt hs)
  where
    fmt :: (String, String) -> String
    fmt (k, v) = k ++ ": " ++ v