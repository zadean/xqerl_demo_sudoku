module namespace _ = "http://xqerl.org/sudoku/create";
(:~ 
 :  Back-tracking a solved board. Removing hints one by one ensuring the board
 :  is still solvable.
 :  The hints that are removed should be 4 at a time for the first ~20 
 :  removals (quad).
 :  After these are removed, go to a by-2 algorithm (diagonal).
 :  Once a puzzle is found and has between 20 and 30 hints, remove single cells.
 :)

import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';

declare namespace notes = "http://xqerl.org/sudoku/notes";

declare variable $_:QUAD  := _:build-mask('quad');
declare variable $_:HORIZ := _:build-mask('horiz');
declare variable $_:VERT  := _:build-mask('vert');
declare variable $_:DIAG  := _:build-mask('diag');
declare variable $_:NONE  := _:build-mask('none');

declare function _:shuffle($items)
{
  random:seeded-permutation(random:integer(), $items)
};

declare function _:p($pos)
{
  let $c1 := $pos?1, $r1 := $pos?2, $c2 := $pos?3, $r2 := $pos?4
  return
    1 + (($c1 - 1) * 3) + ($c2 - 1) + (($r1 - 1) * 27) + ($r2 - 1) * 9
};

declare function _:cell-index($c)
{
  _:p([$c?c1, $c?r1, $c?c2, $c?r2])
};

declare function _:build-mask($type)
{
  let $f := 
    switch ($type)
      case 'quad' return b:mirror-quad#1
      case 'horiz' return b:mirror-horiz#1
      case 'vert' return b:mirror-vert#1
      case 'diag' return b:mirror-diag#1
      default return function($a){$a}
  return
  (
    for $p in 1 to 81
    let $c1  := ($p - 1) mod 9 idiv 3 + 1
      , $c2  := ($p - 1) mod 3 + 1
      , $r1  := ($p - 1) idiv 27 + 1
      , $r2  := ($p - 1) idiv 9 mod 3 + 1
      , $pos := array{$c1, $r1, $c2, $r2}
      , $all := ($f($pos) ! _:p(.))
    return
      map{
        $all[1] : $all
      }
  )
  => map:merge()
};

declare function _:sub-boards($boardString, $cnt)
{
  switch ($cnt)
    case 4 
      return _:sub-boards-1($boardString, $_:QUAD)
    case 2 
      return _:sub-boards-1($boardString, $_:DIAG)
    default 
      return _:sub-boards-1($boardString, $_:NONE)
};

declare function _:sub-boards-1($boardString, $mask)
{
  (
    let $boardNums := string-to-codepoints($boardString) ! (. - 48)
    let $arr := array {$boardNums}
    for $c at $p in $boardNums
    where
      $c ne 0
    let $rem := $mask($p)
    return
      $rem
      => fold-left($arr, function($arr1, $i){
        array:put($arr1, $i, 0)
      })
      => array:flatten() 
      => string-join()
  ) 
  => distinct-values()
  => _:shuffle()
};

declare function _:hint-count($boardString)
{
  (string-to-codepoints($boardString) ! (. - 48))[. ne 0] => count()
};

declare function _:backtrack($string)
{
  _:backtrack-4([$string, 0])
};

declare function _:backtrack-4($string)
{
  let $first := $string?1
                => _:sub-boards(4) 
                => _:first-solvable-simple()
  return
  if (empty($first)) then
    $string 
    => _:backtrack-2()
  else
    _:backtrack-4($first)
};

declare function _:backtrack-2($string)
{
  let $first := $string?1
                => _:sub-boards(2) 
                => _:first-solvable-medium()
  return
  if (empty($first)) then
    $string 
    => _:backtrack-1()
  else
    _:backtrack-2($first)
};

declare function _:backtrack-1($string)
{
  let $first := $string?1 
                => _:sub-boards(1) 
                => _:first-solvable($string?2)
  return
  if (empty($first)) then
    $string
  else
    _:backtrack-1($first)
};

declare function _:first-solvable($strings, $score)
{
  if (empty($strings)) then 
    ()
  else
    let $h := head($strings)
      , $t := tail($strings)
      , $s := $h => b:board-from-string() => s:solve()
    return
      if($s?2 eq 'solved' and $s?1?score gt $score) then
        [$h, $s?1?score]
      else
        _:first-solvable($t, $score)
};

declare function _:first-solvable-simple($strings)
{
  if (empty($strings)) then 
    ()
  else
    let $h := head($strings)
      , $t := tail($strings)
      , $s := $h => b:board-from-string() => s:solve-simple()
    return
      if($s?2 eq 'solved') then
        [$h, $s?1?score]
      else
        _:first-solvable-simple($t)
};

declare function _:first-solvable-medium($strings)
{
  if (empty($strings)) then 
    ()
  else
    let $h := head($strings)
      , $t := tail($strings)
      , $s := $h => b:board-from-string() => s:solve-medium()
    return
      if($s?2 eq 'solved') then
        [$h, $s?1?score]
      else
        _:first-solvable-medium($t)
};

declare 
  %notes:note('unused') 
function _:boxes($board)
{
  for $c1a in 1 to 3, $c1b in (1 to 3) where $c1b ge $c1a
  for $r1a in 1 to 3, $r1b in (1 to 3) where $r1b ge $r1a
  for $c2a in 1 to 3, $c2b in (1 to 3) where if($c1b eq $c1a) then $c2b > $c2a else true()
  for $r2a in 1 to 3, $r2b in (1 to 3) where if($r1b eq $r1a) then $r2b > $r2a else true()
  let $a := $board($c1a)($r1a)($c2a)($r2a)?v
  let $b := $board($c1a)($r1b)($c2a)($r2b)?v
  let $c := $board($c1b)($r1a)($c2b)($r2a)?v
  let $d := $board($c1b)($r1b)($c2b)($r2b)?v
  where
    $a eq $d and $b eq $c
  return
    random:seeded-permutation(
      random:integer(),
      (
        _:map($c1a, $r1a, $c2a, $r2a, $a), 
        _:map($c1a, $r1b, $c2a, $r2b, $b), 
        _:map($c1b, $r1a, $c2b, $r2a, $c), 
        _:map($c1b, $r1b, $c2b, $r2b, $d)
      )
    )[1]
};

declare 
  %notes:note('unused') 
function _:map($c1, $r1, $c2, $r2, $v)
{
  map{ 'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v'  : $v }
};
