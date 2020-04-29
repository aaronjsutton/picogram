-module(picogram_sshd).
-behaviour(ssh_server_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2]).

init(_Args) ->
  {ok, []}.

handle_msg(_Msg, State) ->
  {ok, State}.

handle_ssh_msg({ssh_cm, CM, {data, Channel, _, Data}}, State) ->
  io:format("*** Got data *** ~s~n", [Data]),
  ssh_connection:send(CM, Channel, Data),
  {ok, State}.
