module Parser.Json
 ( parseJson
 ) where

import Model.Json

import Parser
import Data.ByteString as S
import Control.Monad.State
import Data.Char

parseArray :: State S.ByteString [Json]
parseArray = parseArray' []
  where
    parseArray' :: [Json] -> State S.ByteString [Json]
    parseArray' es = do
      json <- parseJson'
      maybeC <- peek
      case maybeC of
        Just ',' -> do
          chomp >> (chompWhile isSeparator)
          parseArray' (es ++ [json])

        _ -> do
          return es

parseJson' :: State S.ByteString Json
parseJson' = do
   maybeC <- peek
   case maybeC of
     Just '"' -> do
       chomp
       word <- chompUntil (== '"')
       chomp
       return (JsonString word)

     Just '[' -> do
       chomp >> (chompWhile isSeparator)
       els <- parseArray
       return (JsonArray els)

     Just c
      | isNumber c -> do
         num <- chompWhile isNumber
         return (JsonInt (read num))

      | otherwise -> return JsonNull

     _ -> return JsonNull

parseJson :: S.ByteString -> Json
parseJson = evalState $ parseJson'