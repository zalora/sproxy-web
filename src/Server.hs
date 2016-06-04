module Server
(
  server
) where

import Control.Exception.Base (throwIO, catch, bracket)
import Data.Bits ((.|.))
import Data.ByteString.Char8 (pack)
import Data.Pool (createPool, destroyAllResources)
import Network.Socket (socket, bind, listen, close, maxListenQueue,
                       getSocketName, inet_addr,
                       Family(AF_UNIX, AF_INET), SocketType(Stream),
                       Socket, SockAddr(SockAddrUnix, SockAddrInet))
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettingsSocket)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (removeLink, setFileMode, socketMode,
                           ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode)
import qualified Database.PostgreSQL.Simple as PG

import Application (app)

type Listen = Either Port FilePath


server :: Listen -> String -> FilePath -> IO ()
server socketSpec connectionString staticDir =
  bracket
    ( do
      sock <- createSocket socketSpec
      pgsql <- createPool
                (PG.connectPostgreSQL $ pack connectionString)
                PG.close
                1 -- stripes
                60 -- keep alive (seconds)
                10 -- max connections
      return (sock, pgsql) )
    ( \(sock, pgsql) -> do
      closeSocket sock
      destroyAllResources pgsql )
    ( \(sock, pgsql) -> do
      listen sock maxListenQueue
      hPutStrLn stderr $ "Static files from `" ++ staticDir ++ "'"
      runSettingsSocket defaultSettings sock =<< app pgsql staticDir )


createSocket :: Listen -> IO Socket
createSocket (Right path) = do
  removeIfExists path
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix path
  setFileMode path $ socketMode
                  .|. ownerWriteMode .|. ownerReadMode
                  .|. groupWriteMode .|. groupReadMode
  hPutStrLn stderr $ "Listening on UNIX socket `" ++ path ++ "'"
  return sock
createSocket (Left port) = do
  sock <- socket AF_INET Stream 0
  addr <- inet_addr "127.0.0.1"
  bind sock $ SockAddrInet (fromIntegral port) addr
  hPutStrLn stderr $ "Listening on localhost:" ++ show port
  return sock


closeSocket :: Socket -> IO ()
closeSocket sock = do
  name <- getSocketName sock
  close sock
  case name of
    SockAddrUnix path -> removeIfExists path
    _ -> return ()


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeLink fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

