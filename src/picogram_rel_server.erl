-module(picogram_rel_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([start_sshd/0, handle_cmd/2]).

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
  {ok, SystemDir} = application:get_env(?OTP_APP, system_dir),
  {ok, UserDir} = application:get_env(?OTP_APP, user_dir),

  logger:info(io_lib:format("==> sshd successfully listening on port ~b, deploying releases to ~s", [Port, RelRoot])),
  {ok, Daemon} = ssh:daemon(Port, [{system_dir, SystemDir},
                                   {user_dir, UserDir},
                                   {subsystems, [ssh_sftpd:subsystem_spec([{root, RelRoot}]), {"rel_sshd", {picogram_sshd, []}}]}]),

  {noreply, #state{active=true, daemon=Daemon, port=Port, rel_root=RelRoot}, hibernate}.

handle_call(root, _, State=#state{rel_root=RelRoot}) ->
  {reply, RelRoot, State}.

%% Public function definitions

start_sshd() ->
  gen_server:cast(?MODULE, start).

% Report the status of the daemon.
handle_cmd(0, _Data) ->
  {0, "rel_server 0.1.0 status: operational"};
  
handle_cmd(1, Data) ->
  Root = gen_server:call(?MODULE, root),
  ok = picogram_phx:deploy(Root, Data),
  {0, io_lib:format("rel_server: installation of phoenix app successful at ~s.", [Data])};

handle_cmd(_, _) ->
  {1, "command not implemented"}.

% Private function definitions
