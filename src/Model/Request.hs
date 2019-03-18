module Model.Request
 ( Method(..)
 , Request(..)
 ) where

import Model.Headers as H
import qualified Data.ByteString as S

data Method = HEAD | OPTIONS | GET | POST | PUT | DELETE deriving (Show, Read)
data Request = Request { method :: Method, path :: String, headers :: H.Headers, body :: S.ByteString } deriving (Show)