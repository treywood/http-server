module Http.Request.Parser
 ( parseRequest
 ) where

import Parser
import Http.Request
import Http.Headers
import qualified Data.ByteString as S
import Data.List
import Data.Char

import Control.Monad.State

parseHeaders :: State S.ByteString Headers
parseHeaders = parseHeaders' []
  where
    parseHeaders' :: Headers -> State S.ByteString Headers
    parseHeaders' hs = do
      line <- chompLine
      chomp
      if (null line) then
        return hs
      else
        let
          name = takeWhile (/= ':') line
          value = drop 2 $ dropWhile (/= ':') line
          header = (name, value)
        in
          parseHeaders' (header:hs)

parseRequest :: S.ByteString -> Request
parseRequest = evalState $ do
  method' <- chompWord
  chomp

  path' <- chompWord
  chomp

  chompLine >> chomp
  headers' <- parseHeaders
  body' <- get
  return Request
    { method = read method'
    , path = path'
    , headers = headers'
    , body = S.tail body'
    }