module namespace _ = 'http://xqerl.org/sudoku/board/permute';

declare variable $_:pos := 
  substring('ABCDEFGHIJKLMNOPQRSTUVWXYZ1234abcdefghijklmnopqrstuvwxyz', ?, 1);

declare variable $_:lookup := (
  let $n := 2 to 9
  for $a in $n
  for $b in $n[. ne $a]
  count $d
  return
  map{
    $a || $b : $_:pos($d),
    $_:pos($d) : $a || $b
  }
) => map:merge();


(: Reorder the board digits to have 1..9 as the top row. :)
declare function _:translate($str as xs:string) as xs:string
{
  let $sub := fn:substring($str, 1, 9)
  return
    fn:translate($str, $sub, '123456789')
};

declare %private function _:split-2($str)
{
  if (string-length($str) eq 0) then ()
  else
  (
    $_:lookup(substring($str, 1, 2)), substring($str, 3) => _:split-2()
  )
};

declare %private function _:split-1($str)
{
  if (string-length($str) eq 0) then ()
  else
  (
    $_:lookup(substring($str, 1, 1)), substring($str, 2) => _:split-1()
  )
};

declare %private function _:strip-1s($string)
{
  (
    for $c in string-to-codepoints($string)
    where
      $c ne 49
    return
      $c
  ) => codepoints-to-string()
};

declare %private function _:add-1s($string)
{
  substring($string, 1 , 3 ) || '1' || 
  substring($string, 4 , 11) || '1' || 
  substring($string, 15, 3 ) || '1' || 
  substring($string, 18, 11) || '1' || 
  substring($string, 29, 11) || '1' || 
  substring($string, 40, 3 ) || '1' || 
  substring($string, 43, 11) || '1' || 
  substring($string, 54, 11) || '1'
};

declare function _:to-id($string)
{
  $string
  => substring(10)
  => _:strip-1s()
  => _:split-2()
  => string-join()
};

declare function _:from-id($id)
{
  '123456789' ||
  $id
  => _:split-1()
  => string-join()
  => _:add-1s()
};

declare %private function _:random_pt_1_3()
{
  random:seeded-permutation(random:integer(), 1 to 3)
};

declare %private function _:random_pt_0_2()
{
  random:seeded-permutation(random:integer(), 0 to 2)
};

declare %private function _:random_pt_9()
{
  random:seeded-permutation(random:integer(), 1 to 9)
};

declare %private function _:random_lt_4()
{
  random:integer(4)
};

declare %private function _:random_lt_2()
{
  random:integer(2)
};

declare %private function _:shuffle($string)
{
  let $new := '0123456789'
  let $perm := '0' || _:random_pt_9() => string-join()
  return
    fn:translate($string, $perm, $new)
};

declare %private function _:do_n($item, $fun, $n)
{
  if ($n le 0) then $item
  else
    _:do_n($fun($item), $fun, $n - 1)
};

declare %private function _:string-to-seq($string)
{
  string-to-codepoints($string) ! (. - 48)
};

declare %private function _:row-array($string)
{
  for tumbling window $w in _:string-to-seq($string)
    start at $s when true() 
    end at $e when ($e - $s) eq 8
  return
    array{$w}
};

declare %private function _:column-array($string)
{
  let $rows := _:row-array($string)
  for $p in 1 to 9
  return
  array{ for $row in $rows return array:get($row, $p)}
};

declare %private function _:permute-map()
{
  (
    for $n at $o in
      for $cs in _:random_pt_0_2()
      for $c0 in _:random_pt_1_3()
      return
        ($cs * 3) + $c0
    return
      map{$o : $n}
  ) => map:merge()
};

declare %private function _:permute-maps()
{
  let $o2 := 0 to 2
  let $o3 := 1 to 3
  for $x in $o2
  for $x1 in $o2[not(. = ($x))]
  for $x2 in $o2[not(. = ($x, $x1))]
  for $y in $o3
  for $y1 in $o3[not(. = ($y))]
  for $y2 in $o3[not(. = ($y, $y1))]
  return
  (
    for $n at $o in
      for $cs in ($x, $x1, $x2)
      for $c0 in ($y, $y1, $y2)
      return
        ($cs * 3) + $c0
    return
      map{$o : $n}
  ) => map:merge()
};

declare %private function _:swap-cols($string)
{
  let $map := _:permute-map()
  return
    _:swap-cols($string, $map)
};

declare %private function _:swap-cols($string, $map)
{
  (
    let $swap := 
    (
      let $cols := _:column-array($string)
      for $p in 1 to 9
      return
        $cols[$map($p)]
    )
    for $p in 1 to 9
    for $row in $swap 
    return 
      array:get($row, $p)
  ) => string-join()
};

declare %private function _:swap-rows($string)
{
  let $map := _:permute-map()
  return
    _:swap-rows($string, $map)
};

declare %private function _:swap-rows($string, $map)
{
  (
    let $rows := _:row-array($string)
    for $p in 1 to 9
    return
      $rows[$map($p)] => array:flatten()
  ) => string-join()
};

declare %private function _:sort-key($array, $map)
{
  (
    let $f := array:flatten($array)
    for $c in 1 to 9
    for $v at $p in $f
    where
      $map($v) eq $c
    return
      $p
  )
  => string-join() 
  => xs:integer()
};

declare variable $_:sort-swap external := _:sort-swap-rows#1;
declare function _:sort-swap-rows($string)
{
  let $val := array{substring($string, 1, 9) => _:string-to-seq()}
  return
  (
    let $rows := _:row-array($string)
    let $fun := _:sort-key(?, $val)
    let $r1 := ($rows[1], $rows[2], $rows[3]) => sort((), $fun)
    let $r2 := ($rows[4], $rows[5], $rows[6]) => sort((), $fun)
    let $r3 := ($rows[7], $rows[8], $rows[9]) => sort((), $fun)
    let $p1 := $fun($r1[1]), 
        $p2 := $fun($r2[1]), 
        $p3 := $fun($r3[1])
    let $rs := 
      if($p1 lt $p2 and $p1 lt $p3) then 
        if ($p2 lt $p3) then ($r1,$r2,$r3) else ($r1,$r3,$r2)
      else if($p2 lt $p1 and $p2 lt $p3) then 
        if ($p1 lt $p3) then ($r2,$r1,$r3) else ($r2,$r3,$r1)
      else 
        if ($p1 lt $p2) then ($r3,$r1,$r2)
        else ($r3,$r2,$r1)
    return
      $rs
      => array:flatten()
  )
  => string-join()
  => _:sort-swap-cols($val)
};

declare %private function _:sort-swap-cols($string, $val)
{
  (
    let $rows := _:column-array($string)
    let $fun := _:sort-key(?, $val)
    let $r1 := ($rows[1], $rows[2], $rows[3]) => sort((), $fun)
    let $r2 := ($rows[4], $rows[5], $rows[6]) => sort((), $fun)
    let $r3 := ($rows[7], $rows[8], $rows[9]) => sort((), $fun)
    let $p1 := $fun($r1[1]), 
        $p2 := $fun($r2[1]), 
        $p3 := $fun($r3[1])
    let $rs := 
      if($p1 lt $p2 and $p1 lt $p3) then 
        if ($p2 lt $p3) then ($r1,$r2,$r3) else ($r1,$r3,$r2)
      else if($p2 lt $p1 and $p2 lt $p3) then 
        if ($p1 lt $p3) then ($r2,$r1,$r3) else ($r2,$r3,$r1)
      else 
        if ($p1 lt $p2) then ($r3,$r1,$r2)
        else ($r3,$r2,$r1)
    return
      for $x in 1 to 9
      for $r in $rs
      return
      $r($x)
  )
  => string-join()
};

declare variable $_:rotate external := _:rotate#1;
declare %private function _:rotate($string)
{
  (
    for $col in _:column-array($string)
    return
      $col => array:flatten() => reverse()
  ) => string-join()
};

declare %private function _:flip-right($string)
{
  (
    for tumbling window $w in _:string-to-seq($string)
      start at $s when true() 
      end at $e when ($e - $s) eq 8
    return
      $w => reverse()
  ) => string-join()
};

declare %private function _:flip-down($string)
{
  _:row-array($string)
  => reverse()
  => string-join()
};

declare %private function _:diag-tr-bl($string)
{
  _:string-to-seq($string)
  => reverse()
  => string-join()
};

declare %private function _:diag-tl-br($string)
{
  $_:rotate($string)
  => _:flip-right()
};

declare function _:random-equal($string)
{
  $string
  => _:swap-cols()
  => _:swap-rows()
  => _:do_n($_:rotate, _:random_lt_4())
  => _:do_n(_:flip-right#1, _:random_lt_2())
  => _:do_n(_:flip-down#1, _:random_lt_2())
  => _:do_n(_:diag-tl-br#1, _:random_lt_2())
  => _:do_n(_:diag-tr-bl#1, _:random_lt_2())
  => _:shuffle()
};

declare function _:minimum-board-string($r)
{
  let $pm := _:permute-maps()
  return
  (
    for $n in 0 to 3
    let $r := _:do_n($r, $_:rotate, $n)
    for $cmap in $pm
    let $r := _:swap-cols($r, $cmap)
    for $rmap in $pm
    let $r := _:swap-rows($r, $rmap)
    group by
      $r
    return
      $r
      => $_:sort-swap()
      => _:translate()
  ) 
  => min()
};
