-module(picogram_sshd).
-behaviour(ssh_server_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-define(CTRL_BYTE, 27).

init(_Args) ->
  {ok, []}.

handle_msg(_Msg, State) ->
  {ok, State}.

handle_ssh_msg({ssh_cm, CM, {
                 data, Channel, _, <<Control:4, Command:4, Data/binary>>}
               }, State) when Control == 13 ->
  io:format("=> sshd process command ~b~n", [Command]),
  {Status, Result} = picogram_rel_server:handle_cmd(Command, Data),
  ssh_connection:send(CM, Channel, [<<Control:4/integer, Status:4/integer>>, list_to_binary(Result)]),
  {ok, State};

handle_ssh_msg({ssh_cm, CM, {data, Channel, _, _Msg}}, State) ->
  ssh_connection:send(CM, Channel, [<<13, 2>>, list_to_binary("Protocol Error: Malformed request")]),
  {ok, State}.

terminate(_Reason, _State) -> ok.
