{-# LANGUAGE FlexibleInstances #-}

module Model.Response
 ( Response(..)
 , serializeResponse
 , Respond(..)
 ) where

import Model.Headers
import Model.Json

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
import Data.List

data Response = Response { status :: Int, headers :: Headers, body :: S.ByteString }

serializeCode :: Int -> String
serializeCode 200 = "200 OK"
serializeCode c = show c

serializeResponse :: Response -> S.ByteString
serializeResponse res =
  let
    len = S.length (body res)
    head = BC.unlines $ map BC.pack
      [ "HTTP/1.1 " ++ serializeCode (status res)
      , serializeHeaders (headers res)
      , "Content-Length: " ++ (show $ len)
      , ""
      ]
  in
    BC.append head (body res)

-- IsResponse

class Respond a where
  respond :: a -> Response

instance Respond Response where
  respond r = r

instance Respond S.ByteString where
  respond str = Response
    { status = 200
    , headers = [("Content-Type", "text/plain")]
    , body = str
    }

instance Respond String where
  respond str = respond (BC.pack str)

instance (Respond a) => Respond (Maybe a) where
  respond (Just r) = respond r
  respond _ = Response
    { status = 404
    , headers = [("Content-Type", "text/plain")]
    , body = BC.pack "Not Found"
    }

instance Respond Json where
  respond json = Response
    { status = 200
    , headers = [("Content-Type", "application/json")]
    , body = BC.pack $ serializeJson json
    }