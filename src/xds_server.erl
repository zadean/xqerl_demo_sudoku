-module(xds_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
handle_call(run, _From, #{run := Run} = State) ->
    Reply = xqerl:run(Run),
    {reply, Reply, State};
handle_call(counts, _From, #{counts := Counts} = State) ->
    Reply = xqerl:run(Counts),
    {reply, Reply, State};
handle_call(puzzles, _From, #{puzzles := Puzzles} = State) ->
    Reply = xqerl:run(Puzzles),
    {reply, Reply, State}.

handle_cast({run, N}, #{run := Run} = State) ->
    _ = [spawn(fun() -> xqerl:run(Run) end ) || _ <- lists:seq(1, N)],
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


load_code() ->
    XqDir = code:priv_dir(xqerl_demo_sudoku),
    B = filename:join(XqDir, "sudoku-boards.xqm"),
    S = filename:join(XqDir, "sudoku-solve.xqm"),
    C = filename:join(XqDir, "sudoku-create.xqm"),
    %io:format("B: ~p~n", [B]),
    %io:format("S: ~p~n", [S]),
    _ = xqerl:compile(B),
    _ = xqerl:compile(S),
    _ = xqerl:compile(C),
    Run = xqerl:compile(filename:join(XqDir, "sudoku-run.xq")),
    Counts = xqerl:compile(filename:join(XqDir, "logic-counts.xq")),
    Puzzles = xqerl:compile(filename:join(XqDir, "get-puzzles.xq")),
    Test = xqerl:compile(filename:join(XqDir, "test.xq")),
    #{test => Test,
      run => Run,
      counts => Counts,
      puzzles => Puzzles}.
