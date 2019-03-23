module Http.Json.Parser
 ( parseJson
 ) where

import Http.Json
import Http.Parser

import qualified Data.ByteString.Lazy as S
import Control.Monad.State
import Data.Char
import Data.List

parseArray :: State S.ByteString (Either String [Json])
parseArray = do
    maybeC <- peek
    case maybeC of
      Just '[' -> do
        chomp
        parseArray' []
      Just c ->
        return $ Left ("Expected '[' got '" ++ [c] ++ "'")
      _      ->
        return $ Left "Expected '[' got end of input"
  where
    parseArray' :: [Json] -> State S.ByteString (Either String [Json])
    parseArray' es = do
      maybeC <- peek
      case maybeC of
        Just ']'      -> chomp >> (return $ Right es)
        Just ','
          | null es   -> return $ Left "unexpected ','"
          | otherwise -> chomp >> next es
        Nothing -> return $ Left "unexpected end of input"
        _ -> next es

    next :: [Json] -> State S.ByteString (Either String [Json])
    next es = do
      chompWhile isSeparator
      result <- parseJson'
      chompWhile isSeparator
      case result of
        Left err    -> return $ Left err
        Right json  -> parseArray' (es ++ [json])

parseObject :: State S.ByteString (Either String [JsonField])
parseObject = do
    maybeC <- peek
    case maybeC of
      Just '{' -> do
        chomp
        parseObject' []
      Just c ->
        return $ Left ("Expected '{' got '" ++ [c] ++ "'")
      _      ->
        return $ Left "Expected '{' got end of input"
  where
    parseObject' :: [JsonField] -> State S.ByteString (Either String [JsonField])
    parseObject' fs = do
      maybeC <- peek
      case maybeC of
        Just '}'      -> chomp >> (return $ Right fs)
        Just ','
          | null fs   -> return $ Left "unexpected ','"
          | otherwise -> chomp >> next fs
        Nothing -> return $ Left "unexpected end of input"
        _ -> next fs

    next :: [JsonField] -> State S.ByteString (Either String [JsonField])
    next fs = do
      chompWhile isSeparator
      parsedName <- parseFieldName
      case parsedName of
        Left err   -> return $ Left err
        Right name -> do
          result <- parseJson'
          chompWhile isSeparator
          case result of
            Left err    -> return $ Left err
            Right value -> parseObject' (fs ++ [(name, value)])

    parseFieldName :: State S.ByteString (Either String String)
    parseFieldName = do
      maybeC <- peek
      case maybeC of
        Just '"' -> do
          chomp
          name <- chompWhile isAlpha
          chomp >> (chompIf (== ':')) >> (chompWhile isSeparator)
          return $ Right name

        Just c -> return $ Left ("unexpected " ++ [c])


parseJson' :: State S.ByteString (Either String Json)
parseJson' = do
   maybeC <- peek
   case maybeC of
     Just '"' -> do
       chomp
       word <- chompUntil (== '"')
       chomp
       return $ Right (JsonString word)

     Just '[' -> do
       result <- parseArray
       case result of
        Right elems -> return $ Right (JsonArray elems)
        Left err    -> return $ Left err

     Just '{' -> do
      result <- parseObject
      case result of
        Right fields -> return $ Right (JsonObject fields)
        Left err     -> return $ Left err

     Just c
      | isNumber c -> do
         num <- chompWhile isNumber
         return $ (Right (JsonInt $ read num))

      | otherwise -> do
        word <- chompWhile isAlpha
        case word of
          "true"  -> return $ Right (JsonBool True)
          "false" -> return $ Right (JsonBool False)
          "null"  -> return $ Right JsonNull
          _       -> return (Left $ "unknown symbol '" ++ word ++ "'")

     _ -> return $ Left "unexpected end of input"

parseJson :: S.ByteString -> Either String Json
parseJson = evalState $ parseJson'