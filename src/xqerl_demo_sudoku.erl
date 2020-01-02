-module(xqerl_demo_sudoku).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         test/0,
         get_puzzles/0,
         create/1,
         counts/0
         ]).

-define(SERVER, xds_server).

test() ->
    gen_server:call(?SERVER, test, 60000).

get_puzzles() ->
    gen_server:call(?SERVER, puzzles).

create(N) ->
    gen_server:cast(?SERVER, {run, N}).

counts() ->
    gen_server:call(?SERVER, counts).


%% ====================================================================
%% Internal functions
%% ====================================================================


