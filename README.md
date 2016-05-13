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

    sproxy-web - Web interface to the sproxy permissions database

      -h         --help, --usage, --version  Display help and version information.
                 --undefok                   Whether to fail on unrecognized command line options.
      -c STRING  --config=STRING             config file (default: sproxy-web.config, from module: Config)


The config file must have the following simplistic structure.

    db_connection_string = "host=127.0.0.1 port=4534 user=alp dbname=alp password=blah"
    port = 8003

