picogram
=====

A tiny continuous deployment manager for my Phoenix projects, written in Erlang.

Usage
-----


### Server

Clone and build:

    $ git clone https://github.com/aaronjsutton/picogram.git
    $ rebar3 release

### Client
  
    $ mix picogram myserver.com port ssh_user_dir release_dir


