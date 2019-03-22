module Http.Server
    ( startServer
    , ServerOptions(..)
    , defaultOptions
    , Routes
    , static
    , sendFile
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import Data.List
import Data.List.Split
import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Char8 as BC
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO
import System.IO.Error

import qualified Http.Request as Req
import Http.Request.Parser
import Http.Response

type Routes = Req.Method -> [String] -> Req.Request -> IO Response
data ServerOptions = ServerOptions { port :: Int, routes :: Routes }

defaultOptions = ServerOptions
  { port = 3000
  , routes = \_ _ _ -> respond $ (Nothing :: Maybe ())
  }

startServer :: ServerOptions -> IO ()
startServer opts = withSocketsDo $ do
    addr <- resolve (show $ port opts)
    E.bracket (open addr) close loop
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        fd <- fdSocket sock
        setCloseOnExecIfNeeded fd
        bind sock (addrAddress addr)
        listen sock 10
        putStrLn $ "Listening at " ++ show (addrAddress addr)
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
        msg <- fmap S.fromStrict (recv conn 1024)
        unless (S.null msg) $ do
          response <- handleRequest msg (routes opts)
          sendAll conn (S.toStrict response)
          close conn

contentTypeForExt :: String -> String
contentTypeForExt "js"    = "application/javascript"
contentTypeForExt "html"  = "text/html"
contentTypeForExt "png"   = "img/png"
contentTypeForExt "jpg"   = "img/jpg"
contentTypeForExt "css"   = "text/css"
contentTypeForExt _       = "text/plain"

sendFile :: String -> IO Response
sendFile path = do
  let ext = last $ splitOn "." path
  result <- E.try $ readFile path
  case result of
    Right contents ->
      return $ response
        { headers = [("Content-Type", contentTypeForExt ext)]
        , body = BC.pack contents
        , gzip = True
        }
    Left err
      | isDoesNotExistError err -> return notFoundResponse
      | otherwise               -> respond (500, E.displayException err)

static :: String -> String -> IO Response
static dir filePath = do
  let fullPath = dir ++ "/" ++ filePath
  sendFile fullPath

handleRequest :: S.ByteString -> Routes -> IO S.ByteString
handleRequest msg routes = do
  let req = parseRequest msg
  putStrLn (show req)
  let pathParts = [ p | p <- splitOn "/" (Req.path req), not (null p) ]
  res <- routes (Req.method req) pathParts req
  return $ serializeResponse res