-module(xds_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).


%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Child = #{id => xds_server,
              start => {xds_server, start_link, []}},
    {ok, {{one_for_all, 0, 1}, [Child]}}.

%%====================================================================
%% Internal functions
%%====================================================================


