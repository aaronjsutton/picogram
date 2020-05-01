%% SSH Interface with a remote release server.
-module(picogram_rel_client).

-export([connect/3, command/3, command/4, transfer/2]).

connect(Host, Port, Options) ->
  ssh:connect(Host, Port, Options).

command(Conn, Command, Timeout) ->
  command(Conn, Command, "", Timeout).

%% Execute a protocol defined command on the server
command(Conn, Command, Data, Timeout) ->
  {ok, Session} = ssh_connection:session_channel(Conn, Timeout),
  success = ssh_connection:subsystem(Conn, Session, "rel_sshd", Timeout),
  ssh_connection:send(Conn, Session, [header(Command), Data]),

  receive
    {ssh_cm, Conn, {data, Session, _, <<13:4, 0:4, Response/binary>>}} ->
      {ok, Response};
    {ssh_cm, Conn, {data, Session, _, <<13:4, Status:4, Response/binary>>}} ->
      {error, io_lib:format("error ~b: ~s~n", [Status, Response])}
  after
    Timeout ->
      {error, "timed out"}
  end.

header(Command) -> <<13:4, Command:4>>.

transfer(Conn, File) ->
  {ok, Channel} = ssh_sftp:start_channel(Conn),
  {ok, Data} = file:read_file(File),
  ok = ssh_sftp:write_file(Channel, filename:basename(File), []).

