(: Re-run all puzzles after a logic change :)
import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';

declare namespace _ = "http://xqerl.org/sudoku/re-run";
declare namespace x = "http://xqerl.org/xquery";

declare variable $_:COLL := 'http://xqerl.org/sudoku/puzzles/run8/';

declare %updating function _:upsert-solved-puzzle($hints, $sol, $score, $node)
{
  let $atts    := map:keys($score) ! attribute {.}{$score(.)}
    , $rec     := element p { $atts, $hints}
  return
    replace node $node with $rec
};

for $p in (# x:parallel unordered #){collection($_:COLL)/s/p}
let $string := $p => data() => tokenize() => head()
  , $board  := $string => b:board-from-string()
  , $solved := $board => s:solve()
  , $solut  := $solved?3 => b:board-string()
  , $score  := $solved?1
return
  _:upsert-solved-puzzle($string, $solut, $score, $p)
