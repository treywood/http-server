{-# LANGUAGE FlexibleInstances #-}

module Http.Response
  ( Response(..)
  , serializeResponse
  , notFoundResponse
  , response
  , Respond(..)
  ) where

import           Http.Headers
import           Http.Json

import           Html                       (Element)

import qualified Codec.Compression.GZip     as GZip
import qualified Data.ByteString.Lazy       as S
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.List

data Response =
  Response
    { status  :: Int
    , headers :: Headers
    , body    :: S.ByteString
    , gzip    :: Bool
    }

statusDescription :: Int -> String
statusDescription 200 = "OK"
statusDescription 201 = "Created"
statusDescription 204 = "No Content"
statusDescription 400 = "Bad Request"
statusDescription 404 = "Not Found"
statusDescription _   = ""

serializeResponse :: Response -> S.ByteString
serializeResponse res = BC.append head content
  where
    (content, encoding) =
      if gzip res
        then (GZip.compress (body res), "gzip")
        else (body res, "identity")
    len = S.length content
    head =
      BC.unlines $
      map
        BC.pack
        [ "HTTP/1.1 " ++
          show (status res) ++ " " ++ statusDescription (status res)
        , serializeHeaders (headers res)
        , "Content-Length: " ++ show len
        , "Content-Encoding: " ++ encoding
        , ""
        ]

response :: Response
response = Response {status = 200, headers = [], body = BC.empty, gzip = False}

notFoundResponse =
  response
    { status = 404
    , headers = [("Content-Type", "text/plain")]
    , body = BC.pack "Not Found"
    }

-- Respond
class Respond a where
  toResponse :: a -> Maybe Response
  respond :: a -> IO Response
  respond a =
    case toResponse a of
      Just r -> return r
      _      -> return notFoundResponse

instance Respond Response where
  toResponse = Just

instance Respond S.ByteString where
  toResponse str =
    Just $ response {headers = [("Content-Type", "text/plain")], body = str}

instance Respond String where
  toResponse str = toResponse (BC.pack str)

instance (Respond a) => Respond (Maybe a) where
  toResponse r = r >>= toResponse

instance Respond Json where
  toResponse json =
    Just $
    response
      { headers = [("Content-Type", "application/json")]
      , body = BC.pack $ serializeJson json
      }

instance Respond () where
  toResponse () = Just $ response {status = 204}

instance (Respond a) => Respond (IO a) where
  toResponse _ = error "Not Implemented"
  respond io = io >>= respond

instance (Respond a, Integral n) => Respond (n, a) where
  toResponse (code, r) = do
    res <- toResponse r
    return res {status = fromIntegral code}

instance (Respond a) => Respond (Either String a) where
  toResponse (Right r)  = toResponse r
  toResponse (Left err) = toResponse (500, err)

instance Respond Element where
  toResponse el =
    Just $
    response
      {headers = [("Content-Type", "text/html")], body = BC.pack $ show el}
