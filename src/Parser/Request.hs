module Parser.Request
 ( parseRequest
 ) where

import Parser
import Model.Request
import Model.Headers
import qualified Data.ByteString as S
import Data.List

import Control.Monad.State

parseHeaders :: State S.ByteString Headers
parseHeaders = parseHeaders' []
  where
    parseHeaders' :: Headers -> State S.ByteString Headers
    parseHeaders' hs = do
      line <- chompLine
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
  path' <- chompWord
  chompLine
  headers' <- parseHeaders
  body' <- chompAll
  return Request
    { method = read method'
    , path = path'
    , headers = headers'
    , body = body'
    }