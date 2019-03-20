module Http.Server
    ( startServer
    , ServerOptions(..)
    , defaultOptions
    , Routes
    ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.List
import Data.List.Split

import Http.Request as Req
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
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk conn = do
        msg <- recv conn 1024
        unless (S.null msg) $ do
          response <- handleRequest msg (routes opts)
          sendAll conn response
          close conn

handleRequest :: S.ByteString -> Routes -> IO S.ByteString
handleRequest msg routes = do
  let req = parseRequest msg
  let pathParts = [ p | p <- splitOn "/" (Req.path req), not (null p) ]
  res <- routes (Req.method req) pathParts req
  return $ serializeResponse res