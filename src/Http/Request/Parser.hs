module Http.Request.Parser
 ( parseRequest
 ) where

import Http.Parser
import Http.Request
import Http.Headers
import qualified Data.ByteString.Lazy as S
import Data.List as List
import Data.Char
import Data.Map as Map
import Network.URI.Encode

import Control.Monad.State

parseHeaders :: State S.ByteString Headers
parseHeaders = parseHeaders' []
  where
    parseHeaders' :: Headers -> State S.ByteString Headers
    parseHeaders' hs = do
      line <- chompLine
      chomp
      if (List.null line) then
        return hs
      else
        let
          name = takeWhile (/= ':') line
          value = List.drop 2 $ dropWhile (/= ':') line
          header = (name, value)
        in
          parseHeaders' (header:hs)

parseQueryString :: State S.ByteString [(String, String)]
parseQueryString = do
    maybeC <- peek
    case maybeC of
      Just '?' -> do
        chomp
        parseQueryString' []
      _ -> return []
  where
    parseQueryString' :: [(String, String)] -> State S.ByteString [(String, String)]
    parseQueryString' qs = do
      pair <- chompUntil (\c -> c == '&' || isSeparator c)
      chompIf (== '&')
      if (List.null pair) then
        return qs
      else
        let
          name = takeWhile (/= '=') pair
          value = tail $ dropWhile (/= '=') pair
          param = (name, decode value)
        in
          parseQueryString' (param:qs)

parseRequest :: S.ByteString -> Request
parseRequest = evalState $ do
  method' <- chompWord
  chomp

  path' <- chompUntil (\c -> c == '?' || isSeparator c)
  queryString' <- parseQueryString

  chompLine >> chomp
  headers' <- parseHeaders
  body' <- get
  return Request
    { method = read method'
    , path = path'
    , headers = Map.fromList headers'
    , query = Map.fromList queryString'
    , body = S.tail body'
    }