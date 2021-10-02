module Http.Parser
 ( ParseResult
 , ParseState
 , chomp
 , chompUntil
 , chompWord
 , chompLine
 , chompAll
 , chompWhile
 , chompIf
 , peek
 , Http.Parser.fail
 , succeed
 ) where

import           Control.Monad.State
import qualified Data.ByteString.Lazy       as S
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Char
import           Data.List

type ParseResult a = Either String a
type ParseState = (S.ByteString, Int)

chomp :: State ParseState (Maybe Char)
chomp = state $ \(str, p) -> case BC.uncons str of
  Just (c, tl) -> (Just c, (tl, p + 1))
  _            -> (Nothing, (BC.empty, p))

chompUntil = chompUntil' ""
  where
    chompUntil' :: String -> (Char -> Bool) -> State ParseState String
    chompUntil' str p = do
      maybeC <- peek
      case maybeC of
        Just c
          | p c       -> return str
          | c == '\n' -> chomp >> chompUntil' str p
          | otherwise -> chomp >> chompUntil' (str ++ [c]) p
        _             -> return str

chompWord = chompUntil isSeparator
chompLine = chompUntil (== '\r')

chompWhile = chompWhile' ""
  where
    chompWhile' :: String -> (Char -> Bool) -> State ParseState String
    chompWhile' str p = do
      maybeC <- peek
      case maybeC of
        Just c
          | p c       -> chomp >> chompWhile' (str ++ [c]) p
          | c == '\n' -> chomp >> chompWhile' str p
          | otherwise -> return str
        _             -> return str

chompAll = chompAll' ""
  where
    chompAll' :: String -> State ParseState String
    chompAll' str = do
      maybeC <- chomp
      case maybeC of
        Just c -> chompAll' (str ++ [c])
        _      -> return str

chompIf :: (Char -> Bool) -> State ParseState (Maybe Char)
chompIf p = do
  maybeC <- peek
  case maybeC of
    Just c
      | p c       -> chomp
      | otherwise -> return Nothing

    _ -> return Nothing

peek :: State ParseState (Maybe Char)
peek = state $ \(str, p) -> case BC.uncons str of
  Just (c, _) -> (Just c, (str, p))
  _           -> (Nothing, (BC.empty, p))

succeed :: a -> State ParseState (ParseResult a)
succeed a = return $ Right a

fail :: String -> State ParseState (ParseResult a)
fail msg = do
  (_, pos) <- get
  return $ Left (msg ++ " at position " ++ show pos)

