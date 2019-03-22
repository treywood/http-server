module Http.QueryString
 ( QueryString
 , WithQueryString(..)
 ) where

type QueryString = [(String, String)]

class WithQueryString a where
  getQueryString :: a -> QueryString

  query :: String -> a -> Maybe String
  query name x =
    case [v | (n, v) <- (getQueryString x), n == name] of
      v : _ -> Just v
      _     -> Nothing