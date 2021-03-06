import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';
import module namespace c = 'http://xqerl.org/sudoku/create' at 'sudoku-create.xqm';

declare namespace _ = "http://xqerl.org/sudoku/run";

declare variable $_:COLL := 'http://xqerl.org/sudoku/puzzles/run8/';

declare %updating function _:upsert-solved-puzzle($hints, $sol, $score)
{
  let $docname := $_:COLL || $sol || '.xml'
    , $ext     := doc-available($docname)
    , $atts    := map:keys($score) ! attribute {.}{$score(.)}
    , $rec     := element p { $atts, $hints}
  return
    if ($ext) then
      insert node $rec into doc($docname)/s
    else
      fn:put(document {element s {attribute solution {$sol}, $rec}},$docname)
};

let $min_id := b:new-seeded-random()
let $string := $min_id?1
               => c:backtrack()
  , $board  := $string?1 => b:board-from-string()
  , $solved := $board => s:solve()
  , $score  := $solved?1
return
  _:upsert-solved-puzzle($string?1, $min_id?2 => trace('id '), $score)
  (: $solved => array:get(3) => b:board-string() :)
