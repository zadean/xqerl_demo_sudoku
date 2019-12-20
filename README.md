xqerl demo - sudoku
=====

A simple Erlang/OTP application for running XQuery compiled with xqerl.

This demo is a sudoku puzzle builder and solver. It will create random,
solvable sudoku puzzles and save them. These puzzles can later be retrieved
for further processing.

All XQuery code is in the `priv` directory. A quick scan through and you may 
notice a lack of XML. "What, I thought this was just an XML query language!?"

Think again!  

Run
-----

    $ rebar3 shell

Usage
-----
With a started shell, a few puzzles can be created with `xqerl_demo_sudoku:create(N)`.
The `N` parameter is the number of processes that should be spawned. Each 
process will eat an entire scheduler for a while.
A note of caution here: It can take minutes for a puzzle to finish. This is 
because random combinations of hints are being tried until a solution comes 
out. After a random solvable puzzle is found, each hint is removed one-by-one 
until a minimum hinted puzzle is found.
With a started shell, a few puzzles can be created with `xqerl_demo_sudoku:create(N).`.
The `N` parameter is the number of processes that should be spawned. Each 
process will eat an entire scheduler for a while.
A note of caution here: It can take minutes for a puzzle to finish. This is 
because random combinations of hints are being tried until a solution comes 
out. After a random solvable puzzle is found, each hint is removed one-by-one 
until a minimum hinted puzzle is found.

Once a few processes have finished, there are puzzles saved. They can be all
be retrieved with `xqerl_demo_sudoku:get_puzzles()`. This will 
return a list of {Solution, Map} with each map key being a difficulty level 
and its value a list of binary() hinted puzzles (or a single binary() if there is only one).

Another interesting thing to do while the CPUs are churning away is to see
how many puzzles are done so far. This can be done with `xqerl_demo_sudoku:counts().` 

Once a few processes have finished, there are puzzles saved. They can be all
be retrieved with `xqerl_demo_sudoku:get_puzzles().`. This will 
return a list of {Solution, Map} with each map key being a difficulty level 
and its value a list of binary() hinted puzzles (or a single binary() if there is only one).

Another interesting thing to do while the CPUs are churning away is to see
how many puzzles are done so far. This can be done with `xqerl_demo_sudoku:counts().` 

