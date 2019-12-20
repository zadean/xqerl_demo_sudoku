module namespace _ = 'http://xqerl.org/sudoku/board';

declare namespace x = "http://xqerl.org/xquery";

(: Reorder the board digits to have 1..9 as the top row. :)
declare function _:translate($str as xs:string) as xs:string
{
  let $sub := fn:substring($str, 1, 9)
  return
    fn:translate($str, $sub, '123456789')
};

declare function _:visible-cells($cell, $all)
{
  for $a in $all
  where
    ($cell?c1 eq $a?c1 and $cell?c2 eq $a?c2) or
    ($cell?r1 eq $a?r1 and $cell?r2 eq $a?r2) or
    ($cell?c1 eq $a?c1 and $cell?r1 eq $a?r1)
  where
    deep-equal($cell, $a) => not()
  return
    $a
};

declare function _:is-possible($board, $c1, $r1, $c2, $r2, $val)
{
  let $map := $board($c1)($r1)($c2)($r2)
  return
    $map?v eq 0 and
    $map?p = $val
};

declare function _:remove-possible($p, $v){
  $p[. ne $v]
};

declare function _:remove-from-possible($map, $val){
  map:put($map, 'p', ($map('p') => _:remove-possible($val)))
};

declare function _:remove-from-possible($board, $c1, $r1, $c2, $r2, $val){
  let $ac1 := $board($c1)
    , $ar1 := $ac1($r1)
    , $ac2 := $ar1($c2)
    , $ar2 := $ac2($r2)
  return
    array:put($board, $c1,
      array:put($ac1, $r1,
        array:put($ar1, $c2,
          array:put($ac2, $r2, 
            _:remove-from-possible($ar2, $val)))))
};

declare function _:set-value($map, $val){
  if ($val eq 0) then
    map:put($map, 'v', $val)
  else
    map:put($map, 'v', $val)
    => map:put('p', ())
};

declare function _:set($board, $c1, $r1, $c2, $r2, $val){
  let $board := _:remove-from-visible($board, $c1, $r1, $c2, $r2, $val)
    , $ac1 := $board($c1)
    , $ar1 := $ac1($r1)
    , $ac2 := $ar1($c2)
    , $ar2 := $ac2($r2)
  return
    $board
    => array:put($c1,
       array:put($ac1, $r1,
         array:put($ar1, $c2,
           array:put($ac2, $r2, _:set-value($ar2, $val)))))
    => _:check-board()
};

declare function _:check-board($board){
  let $bad := (
    for $a in $board => array:flatten()
    where $a('v') eq 0
    where $a('p') => empty()
    return
    $a
  )
  return
  if (empty($bad)) then $board
  else fn:error(xs:QName('_:board'), 'improper board')
};

declare function _:swap($v)
{
  switch ($v)
    case 1 return 3
    case 3 return 1
    default return 2
};

declare function _:distinct($l)
{
  let $f := function($acc, $i){
    if (some $x in $acc satisfies deep-equal($x, $i)) then 
      $acc
    else
      ($acc, $i)
  }
  return
    $l
    => fold-left((), $f)
};

declare function _:mirror-quad($pos)
{
  let $c1 := $pos?1, $r1 := $pos?2, $c2 := $pos?3, $r2 := $pos?4
  return
  (
    [$c1,         $r1,         $c2,         $r2        ],
    [$c1,         _:swap($r1), $c2,         _:swap($r2)],
    [_:swap($c1), $r1,         _:swap($c2), $r2        ],
    [_:swap($c1), _:swap($r1), _:swap($c2), _:swap($r2)]
  ) => _:distinct()
};

declare function _:mirror-horiz($pos)
{
  let $c1 := $pos?1, $r1 := $pos?2, $c2 := $pos?3, $r2 := $pos?4
  return
  (
    [$c1, $r1,         $c2, $r2        ],
    [$c1, _:swap($r1), $c2, _:swap($r2)]
  ) => _:distinct()
};

declare function _:mirror-vert($pos)
{
  let $c1 := $pos?1, $r1 := $pos?2, $c2 := $pos?3, $r2 := $pos?4
  return
  (
    [$c1,         $r1, $c2,         $r2],
    [_:swap($c1), $r1, _:swap($c2), $r2]
  ) => _:distinct()
};

declare function _:mirror-diag($pos)
{
  let $c1 := $pos?1, $r1 := $pos?2, $c2 := $pos?3, $r2 := $pos?4
  return
  (
    [$c1,         $r1,         $c2,         $r2        ],
    [_:swap($c1), _:swap($r1), _:swap($c2), _:swap($r2)]
  ) => _:distinct()
};

declare function _:board-string($board){
  (
    for $p in _:positions()
    let $v := $board($p(1))($p(2))($p(3))($p(4))('v')
    return 
      $v
  )
  => string-join()
};

declare function _:board-string-minus($board, $m){
  (
    for $p in _:positions()
    let $v := $board($p(1))($p(2))($p(3))($p(4))('v')
    return 
      if(deep-equal($p, $m)) then '0' else $v
  )
  => string-join()
};

declare function _:random-value($ns){
  let $c := count($ns)
  where $c ne 0
  let $r := (random:integer($c) + 1)
  return
    $ns[$r]
};

declare function _:random-positions($n, $type){
  random:seeded-permutation(
    random:integer(),
    _:positions()
  )
  => _:fold-positions($n, $type)
};

declare function _:fold-positions($positions, $rem, $type){
  if (empty($positions)) then ()
  else if ($rem le 0) then ()
  else
  let $h := head($positions),
      $ps := 
        switch ($type)
          case 'quad'  return _:mirror-quad($h)
          case 'horiz' return _:mirror-horiz($h)
          case 'vert'  return _:mirror-vert($h)
          case 'diag'  return _:mirror-diag($h)
          default return $h,
      $ct := count($ps)
  return
  (
    $ps,
    _:fold-positions(tail($positions), $rem - $ct, $type)
  )
};

declare function _:block-id($a){
  (($a(1) - 1) * 3) + $a(2)
};
declare function _:column-id($a){
  (($a(1) - 1) * 3) + $a(3)
};
declare function _:row-id($a){
  (($a(2) - 1) * 3) + $a(4)
};

declare function _:remove-from-visible($board, $c1, $r1, $c2, $r2, $val)
{
  let $blk := 
    for $c2 in 1 to 3, $r2 in 1 to 3
    where 
      _:is-possible($board, $c1, $r1, $c2, $r2, $val)
    return
      map{ 'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $val }
  
  let $col := 
    for $r1 in 1 to 3, $r2 in 1 to 3
    where 
      _:is-possible($board, $c1, $r1, $c2, $r2, $val)
    return
      map{ 'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $val }
  
  let $row := 
    for $c1 in 1 to 3, $c2 in 1 to 3
    where 
      _:is-possible($board, $c1, $r1, $c2, $r2, $val)
    return
      map{ 'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $val }
  
  return
    ($blk, $col, $row)
    => fn:fold-left($board, function($acc, $val){
      _:remove-from-possible($acc, $val?c1, $val?r1, $val?c2, $val?r2, $val?v)
    })
};

declare function _:random-order($l){
  random:seeded-permutation(
    random:integer(),
    $l
  )
};

declare function _:positions(){
  let $ns := 1 to 3
  for $r1 in $ns, $r2 in $ns, $c1 in $ns, $c2 in $ns
  return
  [$c1, $r1, $c2, $r2]
};

declare function _:runx($board, $positions){
  if ($positions => empty()) then
    $board
  else
    let $h := fn:head($positions)
    let $ps := $board($h(1))($h(2))($h(3))($h(4))('p')
               => _:random-order()
    return
    _:runx($board, $ps, $positions)
};

declare function _:runx($board, $possibles, $positions){
  if ($possibles => empty()) then
    fn:error()
  else
    let $p := fn:head($possibles)
    let $h := head($positions)
    return
    try {
      let $board1 := _:set($board, $h(1), $h(2), $h(3), $h(4), $p)
      return
      _:runx($board1, tail($positions))
    } catch * {
      _:runx($board, tail($possibles), $positions)
    }
};

declare function _:random-hinted($board, $nr, $type)
{
  (
    for $h in _:random-positions($nr, $type)
    let $c1 := $h(1), $r1 := $h(2), $c2 := $h(3), $r2 := $h(4),
        $v  := ($board($c1)($r1)($c2)($r2)('v'))
    return
    map{
      'c1' : $c1, 'r1' : $r1, 'c2' : $c2,
      'r2' : $r2, 'v'  : $v
    }
  ) => fold-left(_:new(), function($acc, $i){
    _:set($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

(:~ 
  Returns a random solved board.
 :)
declare %public function _:new-random()
{
  _:runx(_:new(), _:positions())
};

(:~ 
  Returns `$count` random solved boards.
 :)
declare %public function _:new-random($count)
{
  (1 to $count) ! _:new-random()
};

(:~ 
  Create a board from a string of 81 values from '0' to '9'.
 :)
declare %public function _:board-from-string($string){
  let $bd := string-to-codepoints($string) ! (. - 48)
  return
    _:new() 
    => _:load($bd)
};

(:~ 
  Returns a new empty board with no hints set.
 :)
declare %private function _:new()
{
  let $empty := map{'p' : 1 to 9, 'v' : 0}
    , $i1 := array{for $a in 1 to 3 return $empty}
    , $i2 := array{for $a in 1 to 3 return $i1}
    , $i3 := array{for $a in 1 to 3 return $i2}
    , $i4 := array{for $a in 1 to 3 return $i3}
  return
    $i4
};

declare %private function _:load($empty, $board){
  (
    for $v at $p in $board
    let $c1 := ($p - 1) mod 9 idiv 3 + 1
      , $c2 := ($p - 1) mod 3 + 1
      , $r1 := ($p - 1) idiv 27 + 1
      , $r2 := ($p - 1) idiv 9 mod 3 + 1
    return
      map{
        'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $v
      }
  )
  => fn:fold-left($empty, function($acc, $val){
    _:set($acc, $val?c1, $val?r1, $val?c2, $val?r2, $val?v)
  })
};
