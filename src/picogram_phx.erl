-module(picogram_phx).

-export([deploy/2]).

deploy(Root, ReleaseTar) ->
  [App | _] = string:split(ReleaseTar, "-"),
  TarPath = filename:join(Root, ReleaseTar),
  AppRoot = binary_to_list(filename:join(Root, App)),
  ok = case filelib:is_dir(AppRoot) of
    true -> 
      phx_stop(AppRoot),
      ok = file:del_dir_r(AppRoot);
    false -> ok
  end,
  ok = file:make_dir(AppRoot),
  ok = erl_tar:extract(TarPath, [{cwd, AppRoot}]),
  ok = phx_start(AppRoot),
  ok = file:delete(TarPath).

phx_stop(AppRoot) -> 
  os:cmd(io_lib:format("~s/bin/~s stop", [AppRoot, filename:basename(AppRoot)])).

phx_start(AppRoot) -> 
  [] = os:cmd(io_lib:format("~s/bin/~s daemon_iex", [AppRoot, filename:basename(AppRoot)])), ok.

phx_pid(AppRoot) -> 
  "0\n" = os:cmd(io_lib:format("~s/bin/~s pid > /dev/null && echo $?", [AppRoot, filename:basename(AppRoot)])).
