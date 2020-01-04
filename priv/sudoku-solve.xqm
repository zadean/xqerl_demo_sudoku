module namespace _ = 'http://xqerl.org/sudoku/solver';

import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';

(: Simple solving logic functions :)
declare variable $_:SIMP_FUNS := (
  ['sole', 0, _:solve-sole-candidate#1],
  ['unique', 1, _:solve-unique-candidate#1],
  ['block', 5, _:solve-block-column#1],
  ['block', 5, _:solve-block-row#1]
);

(: All solving logic functions :)
declare variable $_:LOOP_FUNS := (
  ['sole',             1, _:solve-sole-candidate#1    ],
  ['unique',           1, _:solve-unique-candidate#1  ],
  ['block',            1, _:solve-block-column#1      ],
  ['block',            1, _:solve-block-row#1         ],
  ['naked_2',         10, _:solve-naked-subsets(?, 2) ],
  ['naked_3',         10, _:solve-naked-subsets(?, 3) ],
  ['xy_wing',        100, _:solve-xy-wing#1           ],
  ['x_wing',         100, _:solve-x-wing-row#1        ],
  ['x_wing',         100, _:solve-x-wing-column#1     ],
  ['chain_1',        100, _:solve-singles-chain#1     ],
  ['hidden_2',        10, _:solve-hidden-subsets(?, 2)],
  ['xyz_wing',       100, _:solve-xyz-wing#1          ],
  ['x_cycle',        100, _:solve-x-cycle#1           ],
  ['naked_4',        100, _:solve-naked-subsets(?, 4) ],
    (: ['hidden_3',       100, _:solve-hidden-subsets(?, 3)], :)
    (: ['hidden_4',       100, _:solve-hidden-subsets(?, 4)] :)
  ['wxyz_wing',     1000, _:solve-wxyz-wing#1         ]
  (: ,
  ['coloring',      1000, _:solve-coloring#1          ],
  ['forcing_chain', 1000, _:solve-forcing-chain#1     ] :)

);

(: debug function to dump each step :)
declare function _:dump-diff($original, $new, $key){$new};
declare function _:dump-diff_($original, $new, $key)
{
  let $ms := _:board-diff($original, $new)
  let $f := file:append-text-lines(file:base-dir() || 'dump_do.log', ?)
  let $_ := 
    (
      '_______ ' || $key || ' _______',
      for $m in $ms return $m => _:print-move()) => $f()
  return
  ($_, $new)
};

declare function _:print-move($move)
{
  let $r := (($move?r1 - 1) * 3) + $move?r2 + 64
  let $c := (($move?c1 - 1) * 3) + $move?c2 + 48
  let $v := $move?v
  return
  ($r, $c) => codepoints-to-string() || ' : ' || $v
};

declare function _:set($board, $i)
{
  b:set($board, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
};

declare function _:remove($board, $i)
{
  b:remove-from-possible($board, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
};

(: debug function to print the solved board :)
declare function _:print($board)
{
  for $r1 in 1 to 3
  for $r2 in 1 to 3
  return
  array{
    for $c1 in 1 to 3
    for $c2 in 1 to 3
    return
      $board($c1)($r1)($c2)($r2)?v
  }
};

(: debug function to print the unsolved possible values :)
declare function _:counts($board)
{
  for $r1 in 1 to 3
  for $r2 in 1 to 3
  return
    array {
    for $c1 in 1 to 3
    for $c2 in 1 to 3
    return
      array{
        $board($c1)($r1)($c2)($r2)?p
      }
    }
};

declare %private function _:solved($board) as xs:boolean
{
  _:get-unsolved($board) => empty()
};

declare function _:get-unsolved($board){
  let $ns := 1 to 3
  for $r1 in $ns, $r2 in $ns, $c1 in $ns, $c2 in $ns
  let $c := $board($c1)($r1)($c2)($r2)
  let $p := $c('p')
  let $s := $c('v')
  where
    ($s eq 0)
    and not(empty($p))
  return
    map{
      'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'p' : $p
    }
};

declare %private function _:union($a, $b)
{
  ($a, $b)
  => distinct-values()
  => sort()
};

declare %private function _:intersection($a, $b)
{
  for $x in $b
  where
    $x = $a
  return 
    $x
};

declare %private function _:subtract($a, $b)
{
  for $x in $a
  where
    not($x = $b)
  return 
    $x
};

declare %private function _:deep-subtract($as, $bs)
{
  for $a in $as
  where
    not( some $b in ($bs) satisfies deep-equal($b, $a) )
  return 
    $a
};

declare %private function _:deep-intersection($as, $bs)
{
  for $a in $as
  where
    some $b in $bs satisfies deep-equal($a, $b)
  return
    $a
};

declare %private function _:deep-distinct($as)
{
  _:deep-distinct(tail($as), head($as))
};

declare %private function _:deep-distinct($as, $acc)
{
  if (empty($as)) then $acc
  else if ( some $a in $acc satisfies deep-equal(head($as), $a)) then
    _:deep-distinct(tail($as), $acc)
  else
    _:deep-distinct(tail($as), (head($as), $acc) )
};

declare function _:get-strong-links($un)
{
  let $links := function($cells){
    for $c in $cells, $p in $c?p
    group by $p
    where count($c) eq 2
    return
    [$p, ([$c[1], $c[2]], [$c[2], $c[1]])]
  }
  let $rowlinks := 
    for $r1 in 1 to 3, $r2 in 1 to 3
    return
      $links($un[.?r1 eq $r1 and .?r2 eq $r2])
  let $collinks := 
    for $c1 in 1 to 3, $c2 in 1 to 3
    return
      $links($un[.?c1 eq $c1 and .?c2 eq $c2])
  let $blklinks := 
    for $c1 in 1 to 3, $r1 in 1 to 3
    return
      $links($un[.?c1 eq $c1 and .?r1 eq $r1])
  let $grouped := 
    for $l in ($rowlinks, $collinks, $blklinks)
    let $p := $l?1
    let $c := $l?2
    group by $p
    return
    [$p, $c => _:deep-distinct()]
  return
    $grouped
};

declare function _:get-weak-links($un)
{
  let $links := function($cells){
    for $c in $cells, $p in $c?p
    group by $p
    where count($c) > 2
    return
    [
      $p,
      for $x in $c, $y in $c
      where not(deep-equal($x, $y))
      return 
      [$x, $y]
    ]
  }
  let $rowlinks := 
    for $r1 in 1 to 3, $r2 in 1 to 3
    return
      $links($un[.?r1 eq $r1 and .?r2 eq $r2])
  let $collinks := 
    for $c1 in 1 to 3, $c2 in 1 to 3
    return
      $links($un[.?c1 eq $c1 and .?c2 eq $c2])
  let $blklinks := 
    for $c1 in 1 to 3, $r1 in 1 to 3
    return
      $links($un[.?c1 eq $c1 and .?r1 eq $r1])
  let $grouped := 
    for $l in ($rowlinks, $collinks, $blklinks)
    let $p := $l?1
    let $c := $l?2
    group by $p
    return
    [$p, $c => _:deep-distinct()]
  return
    $grouped
};

declare function _:alternating-cycle($strong, $weak)
{
  if (empty($strong)) then ()
  else
    let $h := head($strong), $t := tail($strong)
    let $a := _:alternating-cycle-strong($h?2, $t, $weak, $h?1)
    return
      if(empty($a)) then
        _:alternating-cycle($t, $weak)
      else
        $a[count(.?1) > 2 and (count(.?1) mod 2) eq 0]
};

declare function _:alternating-cycle-weak($cell, $strong, $weak, $acc)
{
  if(some $x in $acc satisfies deep-equal($x, $cell)) then [$acc] else
  for $s in $strong[deep-equal(.?1, $cell)]
  let $cell1 := $s?2
  let $next := _:deep-subtract($strong, $s)
  return
    _:alternating-cycle-strong($cell1, $next, $weak, ($cell, $acc))
};

declare function _:alternating-cycle-strong($cell, $strong, $weak, $acc)
{
  if(some $x in $acc satisfies deep-equal($x, $cell)) then [$acc] else
  for $s in $weak[deep-equal(.?1, $cell)]
  let $cell1 := $s?2
  let $next := _:deep-subtract($weak, $s)
  return
    _:alternating-cycle-weak($cell1, $strong, $next, ($cell, $acc))
};

(: X Cycle - https://www.sudokuwiki.org/X_Cycles :)
declare function _:solve-x-cycle($board)
{
  let $un := _:get-unsolved($board)
  let $strong := _:get-strong-links($un)
  let $weak := _:get-weak-links($un)
  let $rems := (
    for $s in $strong
    let $p := $s?1
    let $l := $s?2
    let $w := ($weak[.?1 eq $p])?2
    for $cycle in _:alternating-cycle($l, $w)
    let $cycle := $cycle?1
    for $r in (_:deep-subtract($un, $cycle))[.?p = $p]
    where
      count(
        for $c in $cycle 
        where 
          _:can-see-each-other-direct($c, $r) 
        return 1
      ) > 1
    return
      map:put($r, 'v', $p)
  )
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

(: Singles Chain - https://www.sudokuwiki.org/Singles_Chains
  Terminology:
    Single - A link in a row/column/block between two cells that are the 
      only 2 cells with a certain possible value.
    Chain - A chain is a group of links that lead to each other.
      Chains can be loops or not loops. They can also be nets and not 
      a single chain.
  Description:
    Each cell in a link gets put in alternating buckets. Since the link chain
    follows the only 2 possible answers for each cell for a certain number.
    If following a chain leads to a certain bucket containing 2 cells in one
    unit, that bucket is incorrect and all cells in the other bucket are correct.
    The value can be set for each of those cells.
    If the chain does not lead to any set values, each cell in both buckets 
    in different units can be compared for the cells they both can see. These
    cells cannot contain the value, and can have it removed.
 :)
declare function _:solve-singles-chain($board)
{
  let $un := _:get-unsolved($board)
  let $grouped := _:get-strong-links($un)
  let $chains := $grouped => _:single-chains()
  let $sets :=
    for $c in $chains
    return
      _:check-double-link-in-unit($c)
  return
    if (empty($sets)) then
      let $rems :=
        for $c in $chains
        where
          empty($sets)
        return
          _:check-opposite-double-links($c, $un)
      return
        [
          count($rems),
          fn:fold-left($rems, $board, _:remove#2)
        ]
    else
      [
        count($sets),
        fn:fold-left($sets, $board, _:set#2)
      ]
};

(: given list of [poss, cells] return list of [poss, grp1, grp2]
    there can be more than one chain per possible value. (not all linked)
    chains will contain at least 3 values.
 :)
declare function _:single-chains($groups)
{
  for $g in $groups
  let $p := $g?1
  let $g2 := $g?2
  let $ch := _:get-single-chains($g2)
  return
    [$p, $ch]
};

declare function _:get-single-chains($links)
{
  if (empty($links)) then ()
  else
    let $color := 0
    let $h := head($links)
    let $t := tail($links)
    return
      _:get-single-chains($h?2, $t, $h?1, (), $color)
};

declare function _:get-single-chains($current, $rest, $grp1, $grp2, $color)
{
  (: let $_ := trace(count($rest)) return :)
  if ( empty($rest) ) then
  (
    [$grp1, $grp2]
  )
  else if (empty($current)) then
  (
    [$grp1, $grp2], _:get-single-chains($rest)
  )
  else
  (
    let $next := 
      for $c in $current
      let $next := $rest[deep-equal(.?1, $c)]
      return
        $next
    let $rest := _:deep-subtract($rest, $next)
    (: let $_ := trace(count($rest)) :)
    (: let $_ := trace(($next)) :)
    return
      if (empty($next)) then
        ( [$grp1, $grp2], _:get-single-chains($rest) )
      else if ($color eq 1) then
        _:get-single-chains(
          $next?2, 
          $rest, 
          ($grp1, $current) => _:deep-distinct(), 
          $grp2, 
          0)
      else
        _:get-single-chains(
          $next?2, 
          $rest, 
          $grp1, 
          ($grp2, $current) => _:deep-distinct(), 
          1)
  )
};

(: Return (possibly empty) list of values to set :)
declare function _:check-double-link-in-unit($chains)
{
  let $f := function($l){
    some $x in $l, $y in $l 
    satisfies _:can-see-each-other($x, $y)
  }
  let $p := $chains?1
  for $c in $chains?2
  let $g1 := $c?1 (: => trace('g1 ') :)
  let $g2 := $c?2 (: => trace('g2 ') :)
  let $f1 := $f($g1) (: => trace('f1 ') :)
  let $f2 := $f($g2) (: => trace('f2 ') :)
  return
    if($f1 and $f2) then
      ()
    else if($f1) then
      $g2 ! map:put(., 'v', $p)
    else if ($f2) then
      $g1 ! map:put(., 'v', $p)
    else
      ()
};

declare function _:check-opposite-double-links($chains, $un)
{
  let $p := $chains?1
  for $c in $chains?2
  let $g1 := $c?1
  let $g2 := $c?2
  for $ca in _:deep-subtract($un, ($g1, $g2))[.?p = $p]
  where
    some $x in $g1, $y in $g2 
    satisfies 
      _:can-see-each-other($ca, $x) and
      _:can-see-each-other($ca, $y)
  return
    map{'c1' : $ca?c1, 'c2' : $ca?c2, 'r1' : $ca?r1, 'r2' : $ca?r2, 'v' : $p}
};

(: https://sudoku9x9.com/sudoku_solving_techniques_9x9.html :)

(: Hidden Pair/Triplet/Quad
In a Sudoku puzzle, if two digits are candidates for the same two cells in the 
same row and not for any other cells of that row, then such two digits must be 
in either one of these two cells and other candidates in those two cells can be 
eliminated. The same reasoning can be used for any columns and any 3x3 boxes. 
The technique is called hidden pair.

This technique can be applied to more than two digits. It is called hidden 
triplet if three digits are involved and hidden quad if four. The number of 
cells must be the same as the number of digits. But each of the digits 
involved need not be a candidate of all cells. :)
declare %private function _:solve-hidden-subsets($board, $n)
{
  let $a := $board => _:solve-hidden-block-subsets($n)
  let $ac := $a?1, $ab := $a?2
  let $b := $ab => _:solve-hidden-column-subsets($n)
  let $bc := $b?1, $bb := $b?2
  let $c := $bb => _:solve-hidden-row-subsets($n)
  let $cc := $c?1, $cb := $c?2
  return
    [
      $ac + $bc + $cc,
      $cb
    ]
};

declare %private function _:solve-hidden-block-subsets($board, $n)
{
  let $rems :=
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $c1 in $ns, $r1 in $ns
    let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?r1 eq $r1})
    return
      _:paired-hidden($block, $n)
  )
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:solve-hidden-column-subsets($board, $n)
{
  let $rems :=
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $c1 in $ns, $c2 in $ns
    return
      let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?c2 eq $c2})
      return
        _:paired-hidden($block, $n)
  )
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:solve-hidden-row-subsets($board, $n)
{
  let $rems :=
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $r1 in $ns, $r2 in $ns
    return
      let $block := fn:filter($un, function($i){$i?r1 eq $r1 and $i?r2 eq $r2})
      return
        _:paired-hidden($block, $n)
  )
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

(: X-Wing
  If a digit is a candidate for exactly two cells in two different rows and the
  positions (columns) of those two cells in those two different rows are exactly 
  the same, then this digit cannot be a candidate for any other rows in the same 
  two positions (columns). 
 :)
declare %private function _:solve-x-wing-row($board)
{
  let $un := _:get-unsolved($board)
  let $two := (
    for $u in $un
    let $c := (($u?c1 - 1) * 3) + $u?c2
    let $r := (($u?r1 - 1) * 3) + $u?r2
    for $p in $u?p
    group by
      $p, $r
    where
      count($u) eq 2
    order by 
      $p
    return
      [$p, $c, $r, $u])
  let $four :=
    for $x in $two, $y in $two
    let $p1 := $x(1), $p2 := $y(1)
    let $c1 := $x(2), $c2 := $y(2)
    let $r1 := $x(3), $r2 := $y(3)
    let $u1 := $x(4)
    where
      $p1 eq $p2 and
      deep-equal($c1, $c2) and
      $r1 ne $r2
    group by $p1
    where
      count($u1) eq 4
    return 
      [$p1, $u1]
  let $rem := 
  (
    for $x in $four
    return
    let $p := $x(1)
    for $u in $x(2)
    let $c1 := $u?c1
    let $c2 := $u?c2
    let $r := [$u?r1, $u?r2]
    group by
      $p, $c1, $c2
    return
    for $r1 in 1 to 3, $r2 in 1 to 3
    where
      (for $r in $r where deep-equal($r, [$r1, $r2]) return $r) => empty()
    where 
      b:is-possible($board, $c1, $r1, $c2, $r2, $p)
    return
    map{
      'v' : $p, 'c1': $c1, 'c2': $c2, 'r1' : $r1, 'r2' : $r2
    }
  )
  return
    [
      count($rem),
      fn:fold-left($rem, $board, _:remove#2)
    ]
};

declare %private function _:solve-x-wing-column($board)
{
  let $un := _:get-unsolved($board)
  let $two := (
    for $u in $un
    let $c := (($u?c1 - 1) * 3) + $u?c2
    let $r := (($u?r1 - 1) * 3) + $u?r2
    for $p in $u?p
    group by
      $p, $c
    where
      count($u) eq 2
    order by 
      $p
    return
      [$p, $c, $r, $u])
  let $four :=
    for $x in $two, $y in $two
    let $p1 := $x(1), $p2 := $y(1)
    let $c1 := $x(2), $c2 := $y(2)
    let $r1 := $x(3), $r2 := $y(3)
    let $u1 := $x(4)
    where
      $p1 eq $p2 and
      deep-equal($r1, $r2) and
      $c1 ne $c2
    group by $p1
    where
      count($u1) eq 4
    return 
      [$p1, $u1]
  let $rem := 
  (
    for $x in $four
    return
    let $p := $x(1)
    for $u in $x(2)
    let $r1 := $u?r1
    let $r2 := $u?r2
    let $c := [$u?c1, $u?c2]
    group by
      $p, $r1, $r2
    return
    for $c1 in 1 to 3, $c2 in 1 to 3
    where
      (for $c in $c where deep-equal($c, [$c1, $c2]) return $c) => empty()
    where 
      b:is-possible($board, $c1, $r1, $c2, $r2, $p)
    return
    map{
      'v' : $p, 'c1': $c1, 'c2': $c2, 'r1' : $r1, 'r2' : $r2
    }
  )
  return
  [
    count($rem),
    fn:fold-left($rem, $board, _:remove#2)
  ]
};

(: XY-Wing -https://sudoku9x9.com/xy_wing.html
  When 3 cells, XY, XZ, and YZ form a triangle with Z not at the right angle, 
  the cell at the opposite corner cannot contain Z.
 :)
declare %private function _:solve-xy-wing($board)
{
  let $rems :=
  (
    let $un := _:get-unsolved($board)
    for $xy in $un[count(.?p) eq 2]
    for $xz in (
      for $u in b:visible-cells($xy, $un)[count(.?p) eq 2]
      where $u?p = $xy?p
        and deep-equal($u?p, $xy?p) => not()
      return
        $u
    )
    for $yz in (
      for $u in b:visible-cells($xy, $un)[count(.?p) eq 2]
      where 
        deep-equal($u, $xz) => not()
        and $u?p = $xy?p
        and deep-equal($u?p, $xy?p) => not()
        and deep-equal($u?p, $xz?p) => not()
      return
        $u
    )
    let $z := _:intersection($xz?p, $yz?p)
    where 
      count($z) eq 1
      and not($z = $xy?p)
    for $inter in _:deep-intersection(
      b:visible-cells($yz, $un),
      b:visible-cells($xz, $un)
    )
    where
      b:is-possible($board, $inter?c1, $inter?r1, $inter?c2, $inter?r2, $z)
    return 
      map{
        'v' : $z, 
        'r1' : $inter?r1, 'r2' : $inter?r2,
        'c1' : $inter?c1, 'c2' : $inter?c2
      }
  )
  return
  [
    count($rems),
    fn:fold-left($rems, $board, _:remove#2)
  ]
};

declare %private function _:can-see-each-other($cell1, $cell2)
{
  if (deep-equal($cell1, $cell2)) then 
    false()
  else
    ($cell1?c1 eq $cell2?c1 and $cell1?c2 eq $cell2?c2)
    or
    ($cell1?r1 eq $cell2?r1 and $cell1?r2 eq $cell2?r2)
    or
    ($cell1?c1 eq $cell2?c1 and $cell1?r1 eq $cell2?r1)
};

declare %private function _:can-see-each-other-direct($cell1, $cell2)
{
  if (deep-equal($cell1, $cell2)) then 
    false()
  else
    ($cell1?c1 eq $cell2?c1 and $cell1?c2 eq $cell2?c2)
    or
    ($cell1?r1 eq $cell2?r1 and $cell1?r2 eq $cell2?r2)
};

(: WXYZ-Wing - https://www.sudokuwiki.org/WXYZ_Wing
  When 3 cells are visible from a hinge cell and form a naked quad:
  Only one of the possibilities must be unable to see all other cells.
  That value becomes the 'Z' value to eliminate from other cells.
  The 'Z' can be removed from all cells not included in the quad that
  can see all cells in the quad.
 :)
declare %private function _:solve-wxyz-wing($board)
{
  let $un := _:get-unsolved($board)
  let $rems :=
  (
    for $hinge in $un
    let $vis := b:visible-cells($hinge, $un)
    for $quad in _:naked-quad(($hinge, $vis))
    let $ps := $quad?*?p => distinct-values()
    let $cand := 
      for $p in $ps
      for $q in $quad?*
      where
        $q?p = $p
      group by
        $p
      where
        (every $q0 in $q, $q1 in $q satisfies _:can-see-each-other($q0, $q1)) => not()
      return
        [$p, $q]
    return
    if (count($cand) eq 1 ) then
      let $v := $cand?1
      let $head := head($cand?2) => b:visible-cells($un)
      for $inter in fold-left(tail($cand?2), $head, function($acc, $i){
        b:visible-cells($i, $un) => _:deep-intersection($acc)
      })
      where
        (some $w in $cand?2 satisfies deep-equal($w, $inter)) => not()
      where
        b:is-possible($board, $inter?c1, $inter?r1, $inter?c2, $inter?r2, $v)
      return 
        map{
        'v' : $v,
        'r1' : $inter?r1, 'r2' : $inter?r2,
        'c1' : $inter?c1, 'c2' : $inter?c2
      }
    else ()
  )
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

(: XYZ-Wing - https://sudoku9x9.com/xyz_wing.html
  When an XYZ and XZ are in the same block, look for a YZ in a column or row.
  If it exists, remove the Z value from the axis of the cell outside the block
  for the cells in this block.
 :)
declare %private function _:solve-xyz-wing($board)
{
  let $rems := 
  (
    let $un := _:get-unsolved($board)
    for $xyz in $un[count(.?p) eq 3]
    for $xz in (
      for $u in $un[count(.?p) eq 2]
      where $u?c1 eq $xyz?c1
        and $u?r1 eq $xyz?r1
        and deep-equal(_:intersection($u?p, $xyz?p), $u?p)
      return
        $u
    )
    let $xyzc := _:solve-xyz-wing-column($board, $xyz, $xz)
    let $xyzr := _:solve-xyz-wing-row($board, $xyz, $xz)
    return 
      ($xyzc, $xyzr)
  ) => _:deep-distinct()
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:solve-xyz-wing-column($board, $xyz, $xz)
{
  if($xyz?c2 eq $xz?c2) then 
    ()
  else
    let $y as xs:integer := _:subtract($xyz?p, $xz?p)
    let $z := (
      for $r1 in _:subtract((1 to 3), $xyz?r1), $r2 in 1 to 3
      let $cell := $board($xyz?c1)($r1)($xyz?c2)($r2)
      where
        count($cell?p) eq 2
        and $cell?p = $xz?p
        and $cell?p = $y
      return
        _:subtract($cell?p, $y)
    )
    return
      if (empty($z)) then ()
      else
        for $r2 in _:subtract((1 to 3), $xyz?r2)
        where
          b:is-possible($board, $xyz?c1, $xyz?r1, $xyz?c2, $r2, $z)
        return
          map{'c1' : $xyz?c1, 'r1' : $xyz?r1, 'c2' : $xyz?c2, 
              'r2' : $r2, 'v' : $z}
};

declare %private function _:solve-xyz-wing-row($board, $xyz, $xz)
{
  if($xyz?r2 eq $xz?r2) then 
    ()
  else
    let $y := _:subtract($xyz?p, $xz?p)
    let $z := (
      for $c1 in _:subtract((1 to 3), $xyz?c1), $c2 in 1 to 3
      let $cell := $board($c1)($xyz?r1)($c2)($xyz?r2)
      where
        count($cell?p) eq 2
        and $cell?p = $xz?p
        and $cell?p = $y
      return
        _:subtract($cell?p, $y)
    )
    return
      if (empty($z)) then ()
      else
        for $c2 in _:subtract((1 to 3), $xyz?c2)
        where
          b:is-possible($board, $xyz?c1, $xyz?r1, $c2, $xyz?r2, $z)
        return
          map{'c1' : $xyz?c1, 'r1' : $xyz?r1, 'c2' : $c2, 
              'r2' : $xyz?r2, 'v' : $z}
};

(: Coloring = 'run 2 guesses and collect common removals'
  If a digit is a candidate for exactly two cells in a row, in a column or in a 
  3x3 box, then this Coloring technique may be very powerful to make decisions 
  to eliminate a candidata from a cell or to confirm a digit for a cell. 
 :)
declare %private function _:solve-coloring($board)
{
  let $un := _:get-unsolved($board)
  let $col2 := (
    for $u in $un
    let $c1 := $u?c1, $c2 := $u?c2
    for $p in $u?p
    group by
      $c1, $c2, $p
    where
      count($u) eq 2
    order by
      $c1, $c2, $p
    return
      array {$p, $u}
  )
  let $row2 := if(exists($col2)) then () else
  (
    for $u in $un
    let $r1 := $u?r1, $r2 := $u?r2
    for $p in $u?p
    group by
      $r1, $r2, $p
    where
      count($u) eq 2
    order by
      $r1, $r2, $p
    return
      array {$p, $u}
  )
  let $blk2 := if(exists(($col2, $row2))) then () else
  (
    for $u in $un
    let $c1 := $u?c1, $r1 := $u?r1
    for $p in $u?p
    group by
      $c1, $r1, $p
    where
      count($u) eq 2
    order by
      $c1, $r1, $p
    return
      array {$p, $u}
  )
  return
    _:first-coloring($board, ($col2, $row2, $blk2))
};

declare %private function _:first-coloring($board, $cands)
{
  if (empty($cands)) then 
    $board
  else
    let $h := head($cands)
    let $v := $h(1)
    let $p1 := $h(2)
    let $p2 := $h(3)
    let $b1 := b:set($board, $p1?c1, $p1?r1, $p1?c2, $p1?r2, $v) => _:coloring-loop()
    let $b2 := b:set($board, $p2?c1, $p2?r1, $p2?c2, $p2?r2, $v) => _:coloring-loop()
    let $d := _:first-ident-diff($board, $b1, $b2)
    return
      if (empty($d)) then 
        _:first-coloring($board, tail($cands))
      else
        b:remove-from-possible($board, $d?c1, $d?r1, $d?c2, $d?r2, $d?v)
};

declare %private function _:board-diff($original, $new)
{
  for $c1 in 1 to 3, $r1 in 1 to 3, $c2 in 1 to 3, $r2 in 1 to 3
  let $oc := $original($c1)($r1)($c2)($r2)
  let $nc := $new($c1)($r1)($c2)($r2)
  where
    deep-equal($oc, $nc) => not()
  let $rms := if ($oc?v eq $nc?v) then 
        _:subtract($oc?p, $nc?p)
      else 
        _:subtract($oc?p, $nc?v)
  for $rm in $rms
  return
    map{
      'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $rm
    }
};

(: Get first shared diff from the original :)
declare %private function _:first-ident-diff($original, $board1, $board2)
{
  (
    let $diffs1 := _:board-diff($original, $board1)
    let $diffs2 := _:board-diff($original, $board2)
    for $d1 in $diffs1
    for $d2 in $diffs2
    where
      deep-equal($d1, $d2)
    return
      $d1
  ) => head()
};

declare %private function _:coloring-loop($board)
{
  try {
    _:coloring-loop($board, 4)
  } catch * {
     $board
  }
};

declare %private function _:coloring-loop($board, $n)
{
  if ($n eq 0) then
    $board
  else
    let $new := _:do-loop($board, map{}, $_:SIMP_FUNS, $_:SIMP_FUNS)
      , $board1 := $new(1)
    return
      if(_:solved($board1)) then 
        $board1
      else if (deep-equal($board1, $board)) then 
        $board
      else 
        _:coloring-loop($board1, $n - 1)
};

(: Forcing Chain = 'run 2 guesses and collect common removals'
  Forcing Chain is very similar to Coloring. But instead of considering a digit 
  that is a candidate for exactly two cells in a row, in a column or in a 
  3x3 box, we are looking at a cell that has exactly two digits as its 
  candidates. 
 :)
declare %private function _:solve-forcing-chain($board)
{
  let $un := _:get-unsolved($board)
  let $cands := (
    for $u in $un
    where
      $u?p => count() eq 2
    return
      $u
  )
  return
    _:first-forcing-chain($board, $cands)
};

declare %private function _:first-forcing-chain($board, $cands)
{
  if (empty($cands)) then 
    $board
  else
    let $h := head($cands)
    let $b1 := b:set($board, $h?c1, $h?r1, $h?c2, $h?r2, $h?p[1]) 
               => _:coloring-loop()
    let $b2 := b:set($board, $h?c1, $h?r1, $h?c2, $h?r2, $h?p[2]) 
               => _:coloring-loop()
    let $d := _:first-ident-diff($board, $b1, $b2)
    return
      if (empty($d)) then 
        _:first-forcing-chain($board, tail($cands))
      else
        b:remove-from-possible($board, $d?c1, $d?r1, $d?c2, $d?r2, $d?v)
};


(: NOT IMPL.
  Nishio = 'solve until error after guessing...'
  The logic behind Nishio is just like the proof by contradiction method in 
  mathematics. Suppose an undetermined cell is digit "k" say. If this 
  assumption leads to a contradiction, then we can confirm that this assumption 
  is incorrect. As a result this cell cannot be "k" and it can be removed from 
  the cell as a candidate. 
 :)


(: Naked subset
   When combinations of possibilities are in the same scope, they can eliminate
   the possibilities from other cells.
 :)
declare %private function _:solve-naked-subsets($board, $n)
{
  let $a := $board => _:solve-naked-block-subsets($n)
  let $ac := $a?1, $ab := $a?2
  let $b := $ab => _:solve-naked-column-subsets($n)
  let $bc := $b?1, $bb := $b?2
  let $c := $bb => _:solve-naked-row-subsets($n)
  let $cc := $c?1, $cb := $c?2
  return
    [
      $ac + $bc + $cc,
      $cb
    ]
};

declare %private function _:solve-naked-block-subsets($board, $n)
{
  let $rems := 
    (
      let $un := _:get-unsolved($board)
      let $ns := 1 to 3
      for $c1 in $ns, $r1 in $ns
      return
        let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?r1 eq $r1})
        return
          _:paired-possibles($block, $n)
    ) => _:deep-distinct()
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:solve-naked-column-subsets($board, $n)
{
  let $rems := 
    (
      let $un := _:get-unsolved($board)
      let $ns := 1 to 3
      for $c1 in $ns, $c2 in $ns
      return
        let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?c2 eq $c2})
        return
          _:paired-possibles($block, $n)
    ) => _:deep-distinct()
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:solve-naked-row-subsets($board, $n)
{
  let $rems := 
    (
      let $un := _:get-unsolved($board)
      let $ns := 1 to 3
      for $r1 in $ns, $r2 in $ns
      return
        let $block := fn:filter($un, function($i){$i?r1 eq $r1 and $i?r2 eq $r2})
        return
          _:paired-possibles($block, $n)
    ) => _:deep-distinct()
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:naked-quad($cells)
{
  if (count($cells) le 4)  then ()
  else
    for $a in _:permutations($cells, 4)
    let $ms := $a?*
    let $ps := $ms?p => distinct-values()
    where 
      count($ps) eq 4
    return
      array{$ms}
};

declare %private function _:paired-possibles($cells, $n)
{
  if (count($cells) le $n)  then ()
  else
    for $a in _:permutations($cells, $n)
    let $ms := $a?*
    let $ps := $ms?p => distinct-values()
    where 
      count($ps) eq $n
    let $ns := fn:filter($cells, function($i){
      $i?p = $ps and
      (every $x in $ms satisfies deep-equal($i, $x) => not())
    })
    for $p in $ps, $n in $ns
    return
      map:put($n, 'v', $p)
};

declare %private function _:paired-hidden($cells, $n)
{
  if (count($cells) le $n)  then ()
  else
    for $a in _:hidden-permutations($cells, $n)
    let $ps := $a(1)
    let $ms := $a => array:tail() => array:flatten()
    for $m in $ms
    for $mp in $m?p
    where
      not(($mp = $ps))
    return
      map:put($m, 'v', $mp)
};


(: Block and column / Row Interaction
   When any number is only possible in a certain row or column in its block,
   all other cells in that row or column cannot contain that number.
   
   TODO check if possible
 :)
declare %private function _:solve-block-column($board)
{
  let $rems :=
    (
      for $m in (
        for $m in _:get-unsolved($board)
        let $c1 := $m('c1'), $r1 := $m('r1'), $c2 := $m('c2')
        for $p in $m('p')
        group by 
          $c1, $r1, $p
        where 
          count($c2 => distinct-values()) eq 1
        return
          map{
            'c1' : $c1, 'c2' : $c2[1], 'r1' : $r1, 'p' : $p
          })
      let $c1 := $m('c1'), $r1 := $m('r1'), $c2 := $m('c2'), $p := $m('p')
      for $r1 in (1 to 3)[. ne $r1]
      for $r2 in 1 to 3
      where
        b:is-possible($board, $c1, $r1, $c2, $r2, $p)
      return
        map{
          'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $p
        }
    ) => _:deep-distinct()
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

declare %private function _:solve-block-row($board)
{
  let $rems :=
    (
      for $m in (
        for $m in _:get-unsolved($board)
        let $c1 := $m('c1'), $r1 := $m('r1'), $r2 := $m('r2')
        for $p in $m('p')
        group by 
          $c1, $r1, $p
        where 
          count($r2 => distinct-values()) eq 1
        return
          map{
            'c1' : $c1, 'r2' : $r2[1], 'r1' : $r1, 'p' : $p
          })
      let $c1 := $m?c1, $r1 := $m?r1, $r2 := $m?r2, $p := $m?p
      for $c1 in (1 to 3)[. ne $c1]
      for $c2 in 1 to 3
      where
        b:is-possible($board, $c1, $r1, $c2, $r2, $p)
      return
        map{
          'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'v' : $p
        }
    ) => _:deep-distinct()
  return
    [
      count($rems),
      fn:fold-left($rems, $board, _:remove#2)
    ]
};

(: Unique Candidate
  You know that each block, row and column on a Sudoku board must contain every 
  number between 1 and 9. Therefore, if a number, say 4, can only be put in a 
  single cell within a block/column/row, then that number is guaranteed to 
  fit there.
:)
declare %private function _:solve-unique-candidate($board){
  let $un := _:get-unsolved($board)
  let $uc :=
    for $u in $un
    let $c1 := $u?c1, $c2 := $u?c2, $ps := $u?p
    for $p in $ps
    group by 
      $p, $c1, $c2
    where
      count($u) eq 1
    return
      map:put($u, 'v', $p)
  let $ur :=
    for $u in $un
    let $r1 := $u?r1, $r2 := $u?r2, $ps := $u?p
    for $p in $ps
    group by 
      $p, $r1, $r2
    where
      count($u) eq 1
    return
      map:put($u, 'v', $p)
  let $ub :=
    for $u in $un
    let $c1 := $u?c1, $r1 := $u?r1, $ps := $u?p
    for $p in $ps
    group by 
      $p, $c1, $r1
    where
      count($u) eq 1
    return
      map:put($u, 'v', $p)
  let $u := ($uc, $ur, $ub) => _:deep-distinct()
  return
    [
      count($u),
      fold-left($u, $board, _:set#2)
    ]
};

(: Sole Candidate
  When a specific cell can only contain a single number, that number is a 
  "sole candidate". This happens whenever all other numbers but the candidate 
  number exists in either the current block, column or row. 
:)
declare %private function _:solve-sole-candidate($board)
{
  let $s := (
      for $u in _:get-unsolved($board)
      where
        $u?p => count() eq 1
      return
        map{ 'c1' : $u?c1, 'c2' : $u?c2, 'r1' : $u?r1, 'r2' : $u?r2, 'v' : $u?p }
    )
  return
    [
      count($s),
      fold-left($s, $board, _:set#2)
    ]
};

declare %private function _:hidden-permutations($maps, $n)
{
  switch ($n)
    case 2 return 
      _:hidden_permutations_2($maps)
    case 3 return 
      _:hidden_permutations_3($maps)
    case 4 return 
      _:hidden_permutations_4($maps)
    default return ()
};

declare %private function _:permutations($maps, $n)
{
  switch ($n)
    case 2 return 
      _:permutations_2($maps)
    case 3 return 
      _:permutations_3($maps)
    case 4 return 
      _:permutations_4($maps)
    default return ()
};

declare %private function _:permutations_4($maps)
{
  let $cnt := count($maps)
  for $a in 1 to $cnt
    , $b in $a + 1 to $cnt
    , $c in $b + 1 to $cnt
    , $d in $c + 1 to $cnt
  let $m1 := $maps[$a], $m2 := $maps[$b], $m3 := $maps[$c], $m4 := $maps[$d]
  where
    ($m1?p, $m2?p, $m3?p, $m4?p) => distinct-values() => count() eq 4
  return
    [$m1, $m2, $m3, $m4]
};
declare %private function _:permutations_3($maps)
{
  let $cnt := count($maps)
  for $a in 1 to $cnt
    , $b in $a + 1 to $cnt
    , $c in $b + 1 to $cnt
  let $m1 := $maps[$a], $m2 := $maps[$b], $m3 := $maps[$c]
  where
    ($m1?p, $m2?p, $m3?p) => distinct-values() => count() eq 3
  return
    [$m1, $m2, $m3]
};
declare %private function _:permutations_2($maps)
{
  let $cnt := count($maps)
  for $a in 1 to $cnt
    , $b in $a + 1 to $cnt
  let $m1 := $maps[$a], $m2 := $maps[$b]
  where
    ($m1?p, $m2?p) => distinct-values() => count() eq 2
  return
    [$m1, $m2]
};

declare %private function _:hidden_permutations_4($maps)
{
  let $cnt := count($maps)
  for $a in 1 to $cnt
    , $b in $a + 1 to $cnt
    , $c in $b + 1 to $cnt
    , $d in $c + 1 to $cnt
  let $m1 := $maps[$a], $m2 := $maps[$b], $m3 := $maps[$c], $m4 := $maps[$d]
  let $union := $m1?p 
                => _:union($m2?p)
                => _:union($m3?p)
                => _:union($m4?p)
  let $other := $maps[not(position() = ($a, $b, $c, $d))]
  let $us := (
    for $u in $union
    where
      every $x in $other satisfies not($x?p = $u)
    return
      $u
  )  
  where
    $us => count() eq 4
  return
    [$union, $m1, $m2, $m3, $m4]
};
declare %private function _:hidden_permutations_3($maps)
{
  let $cnt := count($maps)
  for $a in 1 to $cnt
    , $b in $a + 1 to $cnt
    , $c in $b + 1 to $cnt
  let $m1 := $maps[$a], $m2 := $maps[$b], $m3 := $maps[$c]
  let $union := $m1?p 
                => _:union($m2?p)
                => _:union($m3?p)
  let $other := $maps[not(position() = ($a, $b, $c))]
  let $us := (
    for $u in $union
    where
      every $x in $other satisfies not($x?p = $u)
    return
      $u
  )  
  where
    $us => count() eq 3
  return
    [$union, $m1, $m2, $m3]
};
declare %private function _:hidden_permutations_2($maps)
{
  let $cnt := count($maps)
  for $a in 1 to $cnt
    , $b in $a + 1 to $cnt
  let $m1 := $maps[$a], $m2 := $maps[$b]
  let $union := $m1?p 
                => _:union($m2?p)
  let $other := $maps[not(position() = ($a, $b))]
  let $us := (
    for $u in $union
    where
      every $x in $other satisfies not($x?p = $u)
    return
      $u
  )
  where
    $us => count() eq 2
  return
    [$us, $m1, $m2]
};

declare %private function _:incr_cnt($cnts, $key, $amt)
{
  let $val  := $cnts($key)
    , $cnt := map:put($cnts, 'score', $cnts('score') + $amt)
  return
    if (empty($val)) then 
      map:put($cnt, $key, 1)
    else 
      map:put($cnt, $key, $val + 1)
};

declare %private function _:do-loop($board, $cnts, $funs, $allFuns)
{
  if ($funs => empty()) then
    [$board, $cnts]
  else
    let $h := head($funs)
      , $key := $h(1)
      , $weight := $h(2)
      , $fn := $h(3)
      , $board1 := try{
                    $board => $fn()
                  } catch * {
                    (: let $_ := trace($err:description, $key) return :)
                    fn:error($err:value)
                  } 
    return
      if ($board1?1 eq 0) then
        (: no change :)
        _:do-loop(
          $board,
          $cnts, 
          tail($funs),
          $allFuns
        )
      else
        _:do-loop(
          _:dump-diff($board, $board1?2, $key), 
          _:incr_cnt($cnts, $key (: => trace('did ') :), $weight * $board1?1),
          $allFuns,
          $allFuns)
};

(:~ 
  Attempts to solve a hinted board.
 :)
declare function _:solve($board)
{
  try {
    _:loop($board, map{'score' : 0})
  } catch * {
    (: let $_ := trace($err:description) return :)
     [map{}, 'incorrect', $board] 
  }
};

declare %private function _:loop($board, $cnts)
{
  let $new := _:do-loop($board, $cnts, $_:LOOP_FUNS, $_:LOOP_FUNS)
    , $board1 := $new(1)
    , $cnts1  := $new(2)
  return
    if(_:solved($board1)) then 
      [$cnts1, 'solved', $board1]
    else if (deep-equal($board1, $board)) then 
      [$cnts, 'unsolved', $board]
    else 
      _:loop($board1, $cnts1)
};

(:~ 
  Attempts to solve a hinted board using only 'line-of-sight' methods.
 :)
declare function _:solve-simple($board)
{
  try {
    _:simp-loop($board, map{'score' : 0})
  } catch * {
     [map{}, 'incorrect', $board] 
  }
};

declare %private function _:simp-loop($board, $cnts)
{
  let $new := _:do-loop($board, $cnts, $_:SIMP_FUNS, $_:SIMP_FUNS)
    , $board1 := $new(1)
    , $cnts1  := $new(2)
  return
    if(_:solved($board1)) then 
      [$cnts1, 'solved', $board1]
    else if (deep-equal($board1, $board)) then 
      [$cnts, 'unsolved', $board]
    else 
      _:simp-loop($board1, $cnts1)
};

