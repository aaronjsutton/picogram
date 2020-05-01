%% A command line interface for picogram deployments.
-module(picogram_cli).

-export([main/2]).

-export([print_version/1, get_mix_version/1, make_tar/1, clean_tar/1, transfer_release/1, connect/1]).

main(Args) -> main([print_version, get_mix_version, make_tar, connect, transfer_release], Args).
main(Steps, Args) ->
  lists:foldl(fun (Step, Ctx) ->
                case Step of
                  {M, F} -> apply(M, F, [Ctx]);
                  F -> apply(?MODULE, F, [Ctx])
                end
            end, dict:from_list(Args), Steps),
  io:format("done. ~n").

print_version(Ctx) ->
  {ok, Vsn} = application:get_key(picogram, vsn),
  io:format("=== picogram v~s~n", [Vsn]),
  Ctx.

get_mix_version(Ctx) ->
  Root = dict:fetch(release_root, Ctx),
  RawVsn = os:cmd(io_lib:format("~s/bin/~s version", [Root, filename:basename(Root)])),
  Vsn = string:trim(string:replace(RawVsn, " ", "-")),
  dict:store(vsn, Vsn, Ctx).

make_tar(Ctx) ->
  Root = dict:fetch(release_root, Ctx),
  Vsn = dict:fetch(vsn, Ctx),
  io:format("=== Compressing release ~s, this make take a minute...~n", [Root]),
  {ok, Filenames} = file:list_dir(Root),
  TarList = lists:map(fun (F) -> {F, filename:absname_join(Root, F)} end,
                      Filenames),
  Tar = filename:join(Root, io_lib:format("~s.tar.gz", [Vsn])),
  erl_tar:create(Tar, TarList, [compress]),
  io:format("=== Output release tar ~s~n", [Tar]),
  dict:store(tar_path, Tar, Ctx).

connect(Ctx) ->
  Host = dict:fetch(host, Ctx),
  Port = dict:fetch(port, Ctx),
  UserDir = dict:fetch(user_dir, Ctx),
  io:format("=== Connecting to rel_server ~s:~b~n", [Host, Port]),
  {ok, Conn} = ssh:connect(Host, Port, [{user_dir, UserDir}]),
  io:format("=== Getting rel_server status...", []),
  {ok, Result} = picogram_rel_client:command(Conn, 0, 1000),
  io:format("\r=== Getting rel_server status... ~s~n", [Result]),
  dict:store(conn, Conn, Ctx).

transfer_release(Ctx) ->
  Conn = dict:fetch(conn, Ctx),
  Tar = dict:fetch(tar_path, Ctx),
  io:format("=== Transferring tar to rel_server, this make take a minute...~n"),
  {ok, Channel} = ssh_sftp:start_channel(Conn),
  {ok, Data} = file:read_file(Tar),
  ssh_sftp:write_file(Channel, filename:basename(Tar), Data),
  io:format("=== Transfer complete.~n"),
  Ctx.

clean_tar(Ctx) ->
  Tar = dict:fetch(tar_path, Ctx),
  io:format("=== Cleaning up local tar...~n"),
  file:delete(Tar),
  Ctx.

