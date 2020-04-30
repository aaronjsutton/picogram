-module(picogram_rel_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2]).
-export([start_sshd/0]).

-record(state, {active=false, daemon, port, rel_root}).

-define(OTP_APP, picogram).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server function definitions

init(_Args) ->
  {ok, #state{}}.

% Conditionally start the SSH daemon. Both a port and a release root must be specified.
% in order to start the server.
handle_cast(start, #state{active=false}) ->
  logger:info("==> Spawing picogram sshd..."),
  {ok, Port} = application:get_env(?OTP_APP, port),
  {ok, RelRoot} = application:get_env(?OTP_APP, rel_root),

  logger:info(io_lib:format("==> sshd successfully listening on port ~b, deploying releases to ~s", [Port, RelRoot])),
  {ok, Daemon} = ssh:daemon(Port, [{system_dir, "priv/ssh/"},
                                   {user_dir, "priv/user/"},
                                   {subsystems, [ssh_sftpd:subsystem_spec([{root, RelRoot}]), {"rel_sshd", {picogram_sshd, []}}]}]),

  {noreply, #state{active=true, daemon=Daemon, port=Port, rel_root=RelRoot}, hibernate}.

%% Public function definitions

start_sshd() ->
  gen_server:cast(?MODULE, start).

% Private function definitions
