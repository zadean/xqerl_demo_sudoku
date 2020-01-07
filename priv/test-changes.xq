import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';

declare namespace x = "http://xqerl.org/xquery";

declare function local:solvable($string)
{
  let $s := $string[1]
            => b:board-from-string()
            => s:solve()
  return
    $s?2 eq 'solved'
};

for $puzz in (# x:parallel unordered #){ collection('http://xqerl.org/sudoku/puzzles/run8')/s/p/data() }
let $s := local:solvable($puzz => tokenize()) => not()
return
  if($s) then $puzz else ()
  
