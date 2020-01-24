-module(xds_server).

-behaviour(gen_server).

-export([code_change/3, 
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         init/1, 
         terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) -> 
    State = load_code(),
    {ok, State}.

handle_call(test, _From, #{test := Test} = State) ->
    Reply = xqerl:run(Test),
    {reply, Reply, State};
handle_call(counts, _From, #{counts := Counts} = State) ->
    Reply = xqerl:run(Counts),
    {reply, Reply, State};
handle_call(puzzles, _From, #{puzzles := Puzzles} = State) ->
    Reply = xqerl:run(Puzzles),
    {reply, Reply, State}.

handle_cast({run, N}, #{run := Run} = State) ->
    Opts =
      #{<<"Q{http://xqerl.org/sudoku/board/permute}rotate">>
       => xds_external:rotate_fun(),
        <<"Q{http://xqerl.org/sudoku/board/permute}sort-swap">>
       => xds_external:sort_swap_fun()},
    _ = [spawn(fun () -> xqerl:run(Run, Opts) end) || _ <- lists:seq(1, N)],
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

load_code() ->
    XqDir = code:priv_dir(xqerl_demo_sudoku),
    % library modules
    P = filename:join(XqDir, "sudoku-board-permute.xqm"),
    B = filename:join(XqDir, "sudoku-boards.xqm"),
    S = filename:join(XqDir, "sudoku-solve.xqm"),
    C = filename:join(XqDir, "sudoku-create.xqm"),
    _ = xqerl:compile(P),
    _ = xqerl:compile(B),
    _ = xqerl:compile(S),
    _ = xqerl:compile(C),
    % main modules
    R = filename:join(XqDir, "sudoku-run.xq"),
    L = filename:join(XqDir, "logic-counts.xq"),
    G = filename:join(XqDir, "get-puzzles.xq"),
    T = filename:join(XqDir, "test.xq"),
    Run = xqerl:compile(R),
    Counts = xqerl:compile(L),
    Puzzles = xqerl:compile(G),
    Test = xqerl:compile(T),
    #{test => Test, 
      run => Run, 
      counts => Counts,
      puzzles => Puzzles}.
