{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (
  main
) where

import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_sproxy_web (getDataDir, version) -- from cabal
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc)
import qualified System.Console.Docopt.NoTH as O

import Server (server)

usage :: IO String
usage = do
  dataDir <- getDataDir
  return $
    "sproxy-web " ++ showVersion version ++
    " web interface to the sproxy database" ++ [qc|

Usage:
  sproxy-web [options]

Options:

  -c, --connstr=CONNSTR    PostgreSQL connection string [default: dbname=sproxy]
  -d, --datadir=DIR        Data directory including static files [default: {dataDir}]

  -s, --socket=SOCK        Listen on this UNIX socket [default: /tmp/sproxy-web.sock]
  -p, --port=PORT          Instead of UNIX socket, listen on this TCP port (localhost)

  -h, --help               Show this message

|]

main :: IO ()
main = do
  doco <- O.parseUsageOrExit =<< usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let
      port = O.getArg args $ O.longOption "port"
      connstr = fromJust $ O.getArg args $ O.longOption "connstr"
      socket = fromJust $ O.getArg args $ O.longOption "socket"
      datadir = fromJust $ O.getArg args $ O.longOption "datadir"
    let listen = case port of
          Nothing -> Right socket
          Just p -> Left $ read p
    server listen connstr datadir

