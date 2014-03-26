sproxy-web
==========

This is the web frontend for the **sproxy** system ([here](http://github.com/zalora/sproxy)). You can get it running in just a few minutes.

## Installing and running `sproxy-web`

First thing: clone this repo, then build this in a sandbox, as usual.

```
$ git clone https://github.com/zalora/sproxy-web.git
$ cd sproxy-web
$ cabal sandbox init
$ cabal install --dependencies-only
$ cabal install
# at this point, you should have a '.cabal-sandbox/bin/sproxy-web' executable
```

Now, let's get serious.

```
$ .cabal-sandbox/bin/sproxy-web --help
sproxy-web - Web interface to the sproxy permissions database

  -h         --help, --usage, --version  Display help and version information.
             --undefok                   Whether to fail on unrecognized command line options.
  -c STRING  --config=STRING             config file (default: , from module: Config)
```

The config file must have the following simplistic structure.

```
$ cat example.config 

db_connection_string = "host=127.0.0.1 port=4534 user=alp dbname=alp password=blah"

# port the web app will listen on
port = 8003
```

## Future releases

We have some improvements to make to the sproxy database to make it smoother to 
manage all the groups, domains, permissions, rules, etc. Be prepared to big changes
in both projects to adapt everything. We also are very aware of the pain points of the current
model and its use.

## Contact

This tool has been written by Alp Mestanogullari <[alp@zalora.com](mailto:alp@zalora.com)>
and is maintained by the same person. Feel free to shoot an email for any comment or suggestion.

Alternatively, you can use [the issues](http://github.com/zalora/sproxy-web). Actually, that would even be better.
