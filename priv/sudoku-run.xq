import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';

declare namespace x = "http://xqerl.org/xquery";
declare namespace _ = "http://xqerl.org/sudoku/run";
(: declare namespace actor = "http://xqerl.org/modules/actor"; :)

(: declare variable $strings external; :)

declare variable $_:QUAD  := _:build-mask('quad');
declare variable $_:HORIZ := _:build-mask('horiz');
declare variable $_:VERT  := _:build-mask('vert');
declare variable $_:DIAG  := _:build-mask('diag');
declare variable $_:NONE  := _:build-mask('none');

declare variable $_:COLL := 'http://xqerl.org/sudoku/puzzles/run8/';

declare %updating function _:upsert-puzzle($maps)
{
  let $sol := ($maps[1])('solution')
  let $docname := $_:COLL || $sol || '.xml'
  let $ext := doc-available($docname)
  let $recs := 
    for $map in $maps
    for $key in map:keys($map)[. ne 'solution']
    let $val := $map($key)
    let $atts := map:keys($val) ! attribute {.}{$val(.)}
    let $rec := element p { $atts, $key}
    return
    $rec
  return
  if ($ext) then
    insert node $recs into doc($docname)/s
  else
    fn:put(document {element s {attribute solution {$sol}, $recs}},$docname)
};

declare function _:run-filename(){
  let $filename := file:base-dir() 
                   || 'out.txt'
  return
    $filename
};

declare function _:board-filename(){
  let $filename := file:base-dir() 
                   || 'board_repo.txt'
  return
    $filename
};

declare function _:p($pos)
{
  let $c1 := $pos?1, $r1 := $pos?2, $c2 := $pos?3, $r2 := $pos?4
  return
  1 + 
  (($c1 - 1) * 3) +
  ($c2 - 1) +
  (($r1 - 1) * 27) + 
  ($r2 - 1) * 9
};

declare function _:build-mask($type)
{
  let $f := switch ($type)
              case 'quad' return b:mirror-quad#1
              case 'horiz' return b:mirror-horiz#1
              case 'vert' return b:mirror-vert#1
              case 'diag' return b:mirror-diag#1
              default return function($a){$a}
  return
  (
    for $p in 1 to 81
    let $c1 := ($p - 1) mod 9 idiv 3 + 1
      , $c2 := ($p - 1) mod 3 + 1
      , $r1 := ($p - 1) idiv 27 + 1
      , $r2 := ($p - 1) idiv 9 mod 3 + 1
    let $pos := array{$c1, $r1, $c2, $r2}
    let $all := ($f($pos) ! _:p(.))
    return
    map{
      $all[1] : $all
    }
  )
  => map:merge()
};

declare function _:map-update-with($map, $fun)
{
  $map
  => map:keys()
  => fold-left($map, function($m, $k){
    let $v := map:get($m, $k)
    return
      $fun($m, $k, $v)
  })
};

declare function _:remove-bad-fun($map, $key, $val)
{
  if ($val instance of map(*)) then
    $map
  else if ($val eq 'bad') then
    map:remove($map, $key)
  else
    $map
};

declare function _:remove-bad($map)
{
  let $ocnt := map:size($map)
  let $map1 := _:map-update-with($map, _:remove-bad-fun#3)
  let $ncnt := map:size($map1)
  (: let $_ := trace($ocnt - $ncnt, 'removed ') :)
  return
    $map1
};

declare function _:more($map)
{
  some $v in $map?*
  satisfies
  typeswitch ($v)
    case xs:string return $v eq 'new'
    default return false()
};

declare function _:sub-boards($boardStrings, $mask)
{
  (
    for $boardString in $boardStrings
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
      => array:flatten() => string-join()
  ) => distinct-values()
};

declare function _:get-hardest-puzzle($all)
{
  let $keys := map:keys($all)
  let $f := function($max, $bKey){
    let $a := $max?2
    let $b := map:get($all, $bKey)
    let $bMax := [$bKey, $b]
    return
      if (empty($max)) then $bMax
      else if (map:size($a) > map:size($b)) then $max
      else if (map:size($a) < map:size($b)) then $bMax
      else if ($a?score > $b?score) then $max
      else if ($a?score < $b?score) then $bMax
      else if ($a?sole > $b?sole) then $max
      else if ($a?sole < $b?sole) then $bMax
      else $max
  }
  return
    $keys
    => fold-left((), $f)
};

declare function _:print($map)
{
  let $s := $map?solution
  let $h := _:get-hardest-puzzle(map:remove($map, 'solution'))
  let $p := $h?1
  let $m := $map($p)
  return
  $p || ',' || $s
};

declare function _:get-first-solved($board, $i, $type){
  let $hint := b:random-hinted($board, $i, $type)
  let $r := s:solve($hint)
  return
  if ($r(2) eq 'solved') then 
    [$hint, $r, $i]
  else if ($r(2) eq 'incorrect') then 
    (: multiple solutions possible, try again with different hints :)
    _:get-first-solved($board, $i, $type)
  else if ($i eq 27 and $type eq 'none') then 
    (: block big hint puzzles and try again :)
    _:get-first-solved($board, 17, $type)
  else if ($i eq 47 and $type eq 'quad') then 
    (: block big hint puzzles and try again :)
    _:get-first-solved($board, 17, $type)
  else if ($i eq 37) then 
    (: block big hint puzzles and try again :)
    _:get-first-solved($board, 17, $type)
  else
    _:get-first-solved($board, $i + 1, $type)
};

declare function _:step($init, $mask)
{
  let $f := function($map, $key, $value){
    if ($value instance of map(*)) then $map
    else if ($value eq 'bad') then
      $map
    else if ($value eq 'new') then
      (
        let $board := b:board-from-string($key)
        let $return := s:solve($board)
        return
          if ($return(2) eq 'solved') then 
            let $next := 
              (_:sub-boards($key, $mask) 
              ! map:entry(., 'new')) => map:merge()
            return
              map:merge((
                map:put($map, $key, $return(1)),
                $next
              ))
          else
            map:put($map, $key, 'bad')
      )
    else
      $map
  }
  return
    _:map-update-with($init, $f)
};

declare function _:loop($map, $mask)
{
  if (_:more($map)) then
    _:step($map, $mask)
    => _:loop($mask)
  else
    _:remove-bad($map)
};

declare function _:init($type)
{
  let $mask := (
    switch ($type)
      case 'quad' return $_:QUAD
      case 'vert' return $_:VERT
      case 'horiz' return $_:HORIZ
      case 'diag' return $_:DIAG
      default return $_:NONE
  )
  let $solution := b:new-random()
  let $randomHinted := _:get-first-solved($solution, 17, $type)
  (: let $_ := 'init done' => trace() :)
  let $hintString := $randomHinted(1) => b:board-string()
  let $hintScore := $randomHinted(2)(1)
  let $next := (_:sub-boards($hintString, $mask) ! map:entry(., 'new')) => map:merge()
  return
    $next 
    => map:put('solution', $solution => b:board-string())
    => map:put($hintString, $hintScore)
};

declare function _:backtrack-init($simplestring, $solution, $type)
{
  let $mask := (
    switch ($type)
      case 'quad' return $_:QUAD
      case 'vert' return $_:VERT
      case 'horiz' return $_:HORIZ
      case 'diag' return $_:DIAG
      default return $_:NONE
  )
  let $randomHinted := b:board-from-string($simplestring) => s:solve()
  (: let $_ := 'init done' => trace() :)
  let $hintScore := $randomHinted(1)
  let $next := (_:sub-boards($simplestring, $mask) ! map:entry(., 'new')) => map:merge()
  return
    $next 
    => map:put('solution', $solution)
    => map:put($simplestring, $hintScore)
};

(: 
  Removes random hints until the board is unsolvable and returns the 
  last solvable version. 
 :)
declare function _:backtrack($solution)
{
  let $perm := random:seeded-permutation(random:integer(),(1 to 81))
  return
    _:backtrack($solution, $perm)
};

declare function _:backtrack($last, $perm)
{
  if (empty($perm)) then $last
  else
    let $boardNums := string-to-codepoints($last) ! (. - 48)
    let $arr := array {$boardNums}
    let $h := head($perm)
    let $t := tail($perm)
    let $next := array:put($arr, $h, 0)
                 => array:flatten()
                 => string-join()
    let $board := b:board-from-string($next)
    let $r := s:solve-simple($board)
    return
      if ($r(2) eq 'solved') then 
        _:backtrack($next, $t)
      else
        _:backtrack($last, $t)
};

(: 
IDEA:
  Run a simple random backtrack on a solved board that only uses the simplest
  of logic to solve a puzzle. Once a board is no longer solvable with the
  simple methods, run it through an exhaustive algorithm that looks at all 
  combinations of cells using all known solving methods (only when under a 
  certain amount of hints, otherwise it could take years to find!). Take the 
  hardest solution available.
 :)

let $string := b:new-random() => b:board-string() => b:translate()
return
  $string
  => _:backtrack()
  => _:backtrack-init($string, 'none')
  => _:loop($_:NONE)
  => _:upsert-puzzle()

