Sproxy Web
==========

This is a web frontend for [Sproxy](https://hackage.haskell.org/package/sproxy) database.

Requirements
============
Sproxy Web is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [sproxy-web.cabal](sproxy-web.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.


Installation
============
    $ git clone https://github.com/zalora/sproxy-web.git
    $ cd sproxy-web
    $ cabal install


Usage
=====
Type `sproxy-web --help` to see usage summary:

    Usage:
      sproxy-web [options]

    Options:

      -c, --connstr=CONNSTR    PostgreSQL connection string [default: dbname=sproxy]
      -d, --datadir=DIR        Data directory including static files [default: <cabal data dir>]

      -s, --socket=SOCK        Listen on this UNIX socket [default: /tmp/sproxy-web.sock]
      -p, --port=PORT          Instead of UNIX socket, listen on this TCP port (localhost)

      -h, --help               Show this message


