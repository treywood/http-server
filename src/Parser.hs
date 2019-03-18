module Parser
 ( chomp
 , chompUntil
 , chompWord
 , chompLine
 , chompAll
 ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
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
      maybeC <- chomp
      case maybeC of
        Just c
          | p c       -> return str
          | c == '\n' -> chompUntil' str p
          | otherwise -> chompUntil' (str ++ [c]) p
        _             -> return str

chompWord = chompUntil isSeparator
chompLine = chompUntil (== '\r')

chompAll = chompAll' ""
  where
    chompAll' :: String -> State S.ByteString String
    chompAll' str = do
      maybeC <- chomp
      case maybeC of
        Just c  -> chompAll' (str ++ [c])
        _       -> return str