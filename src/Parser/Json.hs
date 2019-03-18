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
      chompWhile isSeparator
      
      maybeC <- peek
      case maybeC of
        Just ',' -> do
          chomp >> (chompWhile isSeparator)
          parseArray' (es ++ [json])

        Just ']' -> do
          chomp
          return (es ++ [json])

parseObject :: State S.ByteString [JsonField]
parseObject = parseObject' []
  where
    parseObject' :: [JsonField] -> State S.ByteString [JsonField]
    parseObject' fs = do
      name <- parseFieldName
      chomp >> (chompWhile isSeparator) -- colon

      value <- parseJson'
      chompWhile isSeparator

      maybeC <- peek
      case maybeC of
        Just ',' -> do
          chomp >> (chompWhile isSeparator)
          parseObject' (fs ++ [(name, value)])

        Just '}' -> do
          chomp
          return (fs ++ [(name, value)])

    parseFieldName :: State S.ByteString String
    parseFieldName = do
      maybeC <- peek
      case maybeC of
        Just '"' -> do
          chomp
          name <- chompWhile isAlpha
          chomp
          return name


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
       es <- parseArray
       return (JsonArray es)

     Just '{' -> do
      chomp >> (chompWhile isSeparator)
      fs <- parseObject
      return (JsonObject fs)

     Just c
      | isNumber c -> do
         num <- chompWhile isNumber
         return $ JsonInt (read num)

      | otherwise -> do
        word <- chompWhile isAlpha
        case word of
          "true"  -> return (JsonBool True)
          "false" -> return (JsonBool False)
          "null"  -> return JsonNull
          _       -> return JsonNull

     _ -> return JsonNull

parseJson :: S.ByteString -> Json
parseJson = evalState $ parseJson'