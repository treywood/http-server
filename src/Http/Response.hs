{-# LANGUAGE FlexibleInstances #-}

module Http.Response
 ( Response(..)
 , serializeResponse
 , notFoundResponse
 , Respond(..)
 ) where

import Http.Headers
import Http.Json

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
import Data.List

data Response = Response { status :: Int, headers :: Headers, body :: S.ByteString }

statusDescription :: Int -> String
statusDescription 200 = "OK"
statusDescription 201 = "Created"
statusDescription 204 = "No Content"
statusDescription 400 = "Bad Request"
statusDescription 404 = "Not Found"
statusDescription _ = ""

serializeResponse :: Response -> S.ByteString
serializeResponse res =
  let
    len = S.length (body res)
    head = BC.unlines $ map BC.pack
      [ "HTTP/1.1 " ++ (show $ status res) ++ " " ++ (statusDescription $ status res)
      , serializeHeaders (headers res)
      , "Content-Length: " ++ (show $ len)
      , ""
      ]
  in
    BC.append head (body res)

notFoundResponse = Response
  { status = 404
  , headers = [("Content-Type", "text/plain")]
  , body = BC.pack $ "Not Found"
  }

-- IsResponse

class Respond a where
  toResponse :: a -> Maybe Response

  respond :: a -> IO Response
  respond a = case toResponse a of
    Just r  -> return r
    _       -> return notFoundResponse

instance Respond Response where
  toResponse r = Just r

instance Respond S.ByteString where
  toResponse str = Just $ Response
    { status = 200
    , headers = [("Content-Type", "text/plain")]
    , body = str
    }

instance Respond String where
  toResponse str = toResponse (BC.pack str)

instance (Respond a) => Respond (Maybe a) where
  toResponse r = r >>= toResponse

instance Respond Json where
  toResponse json = Just $ Response
    { status = 200
    , headers = [("Content-Type", "application/json")]
    , body = BC.pack $ serializeJson json
    }

instance Respond () where
  toResponse () = Just $ Response
    { status = 204
    , headers = []
    , body = BC.empty
    }

instance (Respond a) => Respond (IO a) where
  toResponse _ = Nothing -- implementation isn't used
  respond io = io >>= respond