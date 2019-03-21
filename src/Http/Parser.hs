module Http.Parser
 ( chomp
 , chompUntil
 , chompWord
 , chompLine
 , chompAll
 , chompWhile
 , chompIf
 , peek
 ) where

import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List
import Data.Char
import Control.Monad.State

chomp :: State S.ByteString (Maybe Char)
chomp = state $ \str -> case (BC.uncons str) of
  Just (c, tl) -> (Just c, tl)
  _            -> (Nothing, BC.empty)

chompUntil = chompUntil' ""
  where
    chompUntil' :: String -> (Char -> Bool) -> State S.ByteString String
    chompUntil' str p = do
      maybeC <- peek
      case maybeC of
        Just c
          | p c       -> return str
          | c == '\n' -> chomp >> (chompUntil' str p)
          | otherwise -> chomp >> (chompUntil' (str ++ [c]) p)
        _             -> return str

chompWord = chompUntil isSeparator
chompLine = chompUntil (== '\r')

chompWhile = chompWhile' ""
  where
    chompWhile' :: String -> (Char -> Bool) -> State S.ByteString String
    chompWhile' str p = do
      maybeC <- peek
      case maybeC of
        Just c
          | p c       -> chomp >> (chompWhile' (str ++ [c]) p)
          | c == '\n' -> chomp >> (chompWhile' str p)
          | otherwise -> return str
        _             -> return str

chompAll = chompAll' ""
  where
    chompAll' :: String -> State S.ByteString String
    chompAll' str = do
      maybeC <- chomp
      case maybeC of
        Just c  -> chompAll' (str ++ [c])
        _       -> return str

chompIf :: (Char -> Bool) -> State S.ByteString (Maybe Char)
chompIf p = do
  maybeC <- peek
  case maybeC of
    Just c
      | p c       -> chomp
      | otherwise -> return Nothing

    _ -> return Nothing

peek :: State S.ByteString (Maybe Char)
peek = state $ \str -> case (BC.uncons str) of
  Just (c, _)  -> (Just c, str)
  _            -> (Nothing, BC.empty)

expect :: Char -> State S.ByteString (Either String ())
expect char = do
  maybeC <- peek
  case maybeC of
    Just c
      | c == char -> return $ Right ()
      | otherwise -> return (Left $ "expected '" ++ [char] ++ "' but found '" ++ [c])

    _ -> return (Left $ "expected '" ++ [char] ++ "' but got end of input")