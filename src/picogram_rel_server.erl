-module(picogram_rel_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2]).
-export([start/0]).

start_link() ->
  logger:info("Starting picogram release server"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server function definitions

init(_Args) ->
  {ok, []}.

handle_cast(start, State = []) ->
  {ok, Daemon} = ssh:daemon(4005, [{system_dir, "priv/ssh/"},
                                   {user_dir, "priv/user/"},
                                   {subsystems, [ssh_sftpd:subsystem_spec([]), {"rel_sshd", {picogram_sshd, []}}]}]),
  {noreply, [], hibernate}.

%% Public function definitions

start() ->
  gen_server:cast(?MODULE, start).
