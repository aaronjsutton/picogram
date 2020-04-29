%%%-------------------------------------------------------------------
%% @doc picogram public API
%% @end
%%%-------------------------------------------------------------------

-module(picogram_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    picogram_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
