module Http.Json.Parser
 ( parseJson
 ) where

import Http.Json
import Http.Parser as P

import qualified Data.ByteString.Lazy as S
import Control.Monad.State
import Data.Char
import Data.List

parseArray :: State ParseState (ParseResult [Json])
parseArray = do
    maybeC <- peek
    case maybeC of
      Just '[' -> do
        chomp
        parseArray' []
      Just c ->
        P.fail $ "Expected '[' got '" ++ [c] ++ "'"
      _      ->
        P.fail "Expected '[' got end of input"
  where
    parseArray' :: [Json] -> State ParseState (ParseResult [Json])
    parseArray' es = do
      maybeC <- peek
      case maybeC of
        Just ']'      -> chomp >> (P.succeed es)
        Just ','
          | null es   -> P.fail "unexpected ','"
          | otherwise -> chomp >> next es
        Nothing -> P.fail "unexpected end of input"
        _ -> next es

    next :: [Json] -> State ParseState (ParseResult [Json])
    next es = do
      chompWhile isSeparator
      result <- parseJson'
      chompWhile isSeparator
      case result of
        Left err    -> P.fail err
        Right json  -> parseArray' (es ++ [json])

parseObject :: State ParseState (ParseResult [JsonField])
parseObject = do
    maybeC <- peek
    case maybeC of
      Just '{' -> do
        chomp
        parseObject' []
      Just c ->
        P.fail $ "Expected '{' got '" ++ [c] ++ "'"
      _      ->
        P.fail "Expected '{' got end of input"
  where
    parseObject' :: [JsonField] -> State ParseState (ParseResult [JsonField])
    parseObject' fs = do
      maybeC <- peek
      case maybeC of
        Just '}'      -> chomp >> (P.succeed fs)
        Just ','
          | null fs   -> P.fail "unexpected ','"
          | otherwise -> chomp >> next fs
        Nothing -> return $ Left "unexpected end of input"
        _ -> next fs

    next :: [JsonField] -> State ParseState (ParseResult [JsonField])
    next fs = do
      chompWhile isSeparator
      parsedName <- parseFieldName
      case parsedName of
        Left err   -> P.fail err
        Right name -> do
          result <- parseJson'
          chompWhile isSeparator
          case result of
            Left err    -> P.fail err
            Right value -> parseObject' (fs ++ [(name, value)])

    parseFieldName :: State ParseState (ParseResult String)
    parseFieldName = do
      maybeC <- peek
      case maybeC of
        Just '"' -> do
          chomp
          name <- chompWhile isAlpha
          chomp >> (chompIf (== ':')) >> (chompWhile isSeparator)
          P.succeed name

        Just c -> P.fail $ "unexpected " ++ [c]


parseJson' :: State ParseState (ParseResult Json)
parseJson' = do
   maybeC <- peek
   case maybeC of
     Just '"' -> do
       chomp
       word <- chompUntil (== '"')
       chomp
       P.succeed $ JsonString word

     Just '[' -> do
       result <- parseArray
       case result of
        Right elems -> P.succeed $ JsonArray elems
        Left err    -> P.fail err

     Just '{' -> do
      result <- parseObject
      case result of
        Right fields -> P.succeed $ JsonObject fields
        Left err     -> P.fail err

     Just c
      | isNumber c -> do
         num <- chompWhile isNumber
         P.succeed (JsonInt $ read num)

      | otherwise -> do
        word <- chompWhile isAlpha
        case word of
          "true"  -> P.succeed $ JsonBool True
          "false" -> P.succeed $ JsonBool False
          "null"  -> P.succeed JsonNull
          _       -> P.fail $ "unknown symbol '" ++ word ++ "'"

     _ -> P.fail "unexpected end of input"

parseJson :: S.ByteString -> ParseResult Json
parseJson str = evalState parseJson' $ (str, 0)