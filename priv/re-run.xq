(: Re-run all puzzles after a logic change :)
import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';
import module namespace p = 'http://xqerl.org/sudoku/board/permute' at 'sudoku-board-permute.xqm';
import module namespace c = 'http://xqerl.org/sudoku/create' at 'sudoku-create.xqm';


declare namespace _ = "http://xqerl.org/sudoku/re-run";

declare variable $_:COLL := 'http://xqerl.org/sudoku/puzzles/run8/';

declare %updating function _:upsert-solved-puzzle($hints, $sol, $score)
{
  let $docname := $_:COLL || $sol || '.xml'
    , $atts    := map:keys($score) ! attribute {.}{$score(.)}
    , $rec     := element p { $atts, $hints}
  return
    insert node $rec into doc($docname)/s
};

(: for $s in collection($_:COLL)/s :)
(: for $s in doc($_:COLL||'Qkt4z4fX2aMfKmlQETwKegQFo2DLgCdU.xml')/s :)
let $sol := 'Qkt4z4fX2aMfKmlQETwKegQFo2DLgCdU'
let $id := p:from-id($sol)
(: let $id := p:from-id($s/@solution) :)
let $string := $id => c:backtrack()
  , $board  := $string?1 => b:board-from-string()
  , $solved := $board => s:solve()
  , $solut  := $solved?3 => b:board-string()
  , $score  := $solved?1
return
  _:upsert-solved-puzzle($string?1, $sol, $score)
