{-# LANGUAGE FlexibleInstances #-}

module Http.Response
 ( Response(..)
 , serializeResponse
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

-- IsResponse

class Respond a where
  toResponse :: a -> Response

  respond :: a -> IO Response
  respond a = return $ toResponse a

instance Respond Response where
  toResponse r = r

instance Respond S.ByteString where
  toResponse str = Response
    { status = 200
    , headers = [("Content-Type", "text/plain")]
    , body = str
    }

instance Respond String where
  toResponse str = toResponse (BC.pack str)

instance (Respond a) => Respond (Maybe a) where
  toResponse (Just r) = toResponse r
  toResponse _ = Response
    { status = 404
    , headers = [("Content-Type", "text/plain")]
    , body = BC.pack "Not Found"
    }

instance Respond Json where
  toResponse json = Response
    { status = 200
    , headers = [("Content-Type", "application/json")]
    , body = BC.pack $ serializeJson json
    }

instance Respond () where
  toResponse () = Response
    { status = 204
    , headers = []
    , body = BC.empty
    }

instance (Respond a) => Respond (IO a) where
  -- implementation isn't used
  toResponse _ = Response { status = 404, headers = [], body = BC.empty }

  respond io = do
    res <- io
    respond res