module Http.Request.Parser
 ( parseRequest
 ) where

import qualified Data.ByteString.Lazy as S
import           Data.Char
import           Data.List            as List
import           Data.Map             as Map
import           Http.Headers
import           Http.Parser
import           Http.Request
import           Network.URI.Encode

import           Control.Monad.State

parseHeaders :: State ParseState Headers
parseHeaders = parseHeaders' []
  where
    parseHeaders' :: Headers -> State ParseState Headers
    parseHeaders' hs = do
      line <- chompLine
      chomp
      if List.null line then
        return hs
      else
        let
          name = takeWhile (/= ':') line
          value = List.drop 2 $ dropWhile (/= ':') line
          header = (name, value)
        in
          parseHeaders' (header:hs)

parseQueryString :: State ParseState [(String, String)]
parseQueryString = do
    maybeC <- peek
    case maybeC of
      Just '?' -> do
        chomp
        parseQueryString' []
      _ -> return []
  where
    parseQueryString' :: [(String, String)] -> State ParseState [(String, String)]
    parseQueryString' qs = do
      pair <- chompUntil (\c -> c == '&' || isSeparator c)
      chompIf (== '&')
      if List.null pair then
        return qs
      else
        let
          name = takeWhile (/= '=') pair
          value = tail $ dropWhile (/= '=') pair
          param = (name, decode value)
        in
          parseQueryString' (param:qs)

parseRequest' :: State ParseState Request
parseRequest' = do
    method' <- chompWord
    chomp

    path' <- chompUntil (\c -> c == '?' || isSeparator c)
    queryString' <- parseQueryString

    chompLine >> chomp
    headers' <- parseHeaders
    (body', _) <- get
    return Request
      { method = read method'
      , path = path'
      , headers = Map.fromList headers'
      , query = Map.fromList queryString'
      , body = S.tail body'
      }

parseRequest :: S.ByteString -> Request
parseRequest str = evalState parseRequest' (str, 0)

