-module(xqerl_demo_sudoku).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         get_puzzles/0,
         create/1,
         counts/0
         ]).

-define(SERVER, xds_server).

get_puzzles() ->
    gen_server:call(?SERVER, puzzles).

create(N) ->
    gen_server:cast(?SERVER, {run, N}).

counts() ->
    gen_server:call(?SERVER, counts).


%% ====================================================================
%% Internal functions
%% ====================================================================


