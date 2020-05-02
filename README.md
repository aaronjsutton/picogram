picogram
=====

A tiny continuous deployment manager for my Phoenix projects, written in Erlang.

Not excatly workable yet, a port of a few escripts I wrote for another project.

Usage
-----


### Server

Clone and build:

    $ git clone https://github.com/aaronjsutton/picogram.git
    $ rebar3 release

### Client
  
    $ mix picogram myserver.com port ssh_user_dir release_dir


