-module(picogram_phx).

-export([deploy/2]).

deploy(Root, ReleaseTar) ->
  [App | _] = string:split(ReleaseTar, "-"),
  TarPath = filename:join(Root, ReleaseTar),
  AppRoot = binary_to_list(filename:join(Root, App)),
  EnvFile = filename:join(AppRoot, ".picorc"),

  {ok, EnvData} = file:read_file(EnvFile),
  Env = parse_env(EnvData),
  ok = case filelib:is_dir(AppRoot) of
    true -> 
      phx_stop(AppRoot),
      ok = file:del_dir_r(AppRoot);
    false -> ok
  end,
  ok = file:make_dir(AppRoot),
  file:write_file(EnvFile, EnvData),
  ok = erl_tar:extract(TarPath, [{cwd, AppRoot}]),
  ok = phx_start(AppRoot, Env),
  ok = file:delete(TarPath).

phx_stop(AppRoot) -> 
  os:cmd(io_lib:format("~s/bin/~s stop", [AppRoot, filename:basename(AppRoot)])).

phx_start(AppRoot, Env) -> 
  [] = os:cmd(io_lib:format("~s ~s/bin/~s daemon_iex", [cmd_preamble(Env), AppRoot, filename:basename(AppRoot)])), ok.

phx_pid(AppRoot) -> 
  "0\n" = os:cmd(io_lib:format("~s/bin/~s pid > /dev/null && echo $?", [AppRoot, filename:basename(AppRoot)])).

parse_env(Data) ->
  Lines = string:split(string:trim(Data), "\n", all),
  lists:map(fun(Line) -> [K, V] = string:split(Line, "="), {K, V} end, Lines).

cmd_preamble(Env) -> 
  lists:foldl(fun({K, V}, Acc) -> 
                  String = io_lib:format("export ~s=~s; ", [K, V]), string:concat(Acc, String) end
              , "", Env).
