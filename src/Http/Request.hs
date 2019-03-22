module Http.Request
 ( Method(..)
 , Request(..)
 , WithQueryString(..)
 , WithHeaders(..)
 ) where

import Http.Headers as H
import Http.QueryString as Q
import qualified Data.ByteString.Lazy as S

data Method = HEAD | OPTIONS | GET | POST | PUT | DELETE deriving (Show, Read, Eq)
data Request = Request
                { method :: Method
                , path :: String
                , headers :: H.Headers
                , queryString :: Q.QueryString
                , body :: S.ByteString
                } deriving (Show, Eq)

instance WithHeaders Request where
  getHeaders req = headers req

instance WithQueryString Request where
  getQueryString req = (queryString req)