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
  ['naked_2',         10, _:solve-naked-subsets(?, 2) ],
  ['naked_3',         10, _:solve-naked-subsets(?, 3) ],
  ['block',           10, _:solve-block-column#1      ],
  ['block',           10, _:solve-block-row#1         ],
  ['hidden_2',        10, _:solve-hidden-subsets(?, 2)],
  ['xyz_wing',       100, _:solve-xyz-wing#1          ],
  ['x_wing',         100, _:solve-x-wing-row#1        ],
  ['xy_wing',        100, _:solve-xy-wing#1           ],
  ['hidden_3',       100, _:solve-hidden-subsets(?, 3)],
  ['naked_4',        100, _:solve-naked-subsets(?, 4) ],
  ['hidden_4',       100, _:solve-hidden-subsets(?, 4)],
  ['coloring',      1000, _:solve-coloring#1          ],
  ['forcing_chain', 1000, _:solve-forcing-chain#1     ]

);

(: debug function to dump each step :)
declare function _:dump($board, $key)
{
  (: let $f := file:append-text-lines(file:base-dir() || 'dump_do.log', ?) :)
  (: let $_ := array {$board => _:print(), $key} => serialize(map{'method':'adaptive'}) => $f() :)
  (: return :)
  $board
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

declare %private function _:get-unsolved($board){
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

declare %private function _:deep-intersection($a, $b)
{
  for $a in $a
  where
    some $b in $b satisfies deep-equal($a, $b)
  return
    $a
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
  $board
  => _:solve-hidden-block-subsets($n)
  => _:solve-hidden-column-subsets($n)
  => _:solve-hidden-row-subsets($n)
};

declare %private function _:solve-hidden-block-subsets($board, $n)
{
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $c1 in $ns, $r1 in $ns
    let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?r1 eq $r1})
    return
      _:paired-hidden($block, $n)
  )
  => fn:fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

declare %private function _:solve-hidden-column-subsets($board, $n)
{
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $c1 in $ns, $c2 in $ns
    return
      let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?c2 eq $c2})
      return
        _:paired-hidden($block, $n)
  )
  => fn:fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

declare %private function _:solve-hidden-row-subsets($board, $n)
{
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $r1 in $ns, $r2 in $ns
    return
      let $block := fn:filter($un, function($i){$i?r1 eq $r1 and $i?r2 eq $r2})
      return
        _:paired-hidden($block, $n)
  )
  => fn:fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
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
    for $p in $u?p
    let $r1 := $u?r1, $r2 := $u?r2
    let $c := (($u?c1 - 1) * 3) + $u?c2
    let $r := (($u?r1 - 1) * 3) + $u?r2
    group by
      $p,
      $r
    where
      count($u) eq 2
    order by $p
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
    where b:is-possible($board, $c1, $r1, $c2, $r2, $p)
    return
    map{
      'v' : $p, 'c1': $c1, 'c2': $c2, 'r1' : $r1, 'r2' : $r2
    }
  )
  return
  $rem
  => fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

declare %private function _:solve-x-wing-column($board)
{
  (: TODO :)
};

(: XY-Wing -https://sudoku9x9.com/xy_wing.html
  When 3 cells, XY, XZ, and YZ form a triangle with Z not at the right angle, 
  the cell at the opposite corner cannot contain Z.
 :)
declare %private function _:solve-xy-wing($board)
{
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
  => fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

(: XYZ-Wing - https://sudoku9x9.com/xyz_wing.html
  When an XYZ and XZ are in the same block, look for a YZ in a column or row.
  If it exists, remove the Z value from the axis of the cell outside the block
  for the cells in this block.
 :)
declare %private function _:solve-xyz-wing($board)
{
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
    return 
      [$xyz, $xz]
  )
  => fold-left($board, function($acc, $i){
    $acc
    => _:solve-xyz-wing-column($i(1), $i(2))
    => _:solve-xyz-wing-row($i(1), $i(2))
  })
};

declare %private function _:solve-xyz-wing-column($board, $xyz, $xz)
{
  if($xyz?c2 eq $xz?c2) then 
    $board 
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
      if (empty($z)) then $board
      else
        _:subtract((1 to 3), $xyz?r2)
        => fold-left($board, function($acc, $r2){
          b:remove-from-possible($acc, $xyz?c1, $xyz?r1, $xyz?c2, $r2, $z)
        })
};

declare %private function _:solve-xyz-wing-row($board, $xyz, $xz)
{
  if($xyz?r2 eq $xz?r2) then 
    $board 
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
      if (empty($z)) then $board
      else
        _:subtract((1 to 3), $xyz?c2)
        => fold-left($board, function($acc, $c2){
          b:remove-from-possible($acc, $xyz?c1, $xyz?r1, $c2, $xyz?r2, $z)
        })
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
  let $row2 := (
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
  let $blk2 := (
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
     $board => _:dump('co ' || $err:code)
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
  $board
  => _:solve-naked-block-subsets($n)
  => _:solve-naked-column-subsets($n)
  => _:solve-naked-row-subsets($n)
};

declare %private function _:solve-naked-block-subsets($board, $n)
{
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $c1 in $ns, $r1 in $ns
    return
      let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?r1 eq $r1})
      return
        _:paired-possibles($block, $n)
  )
  => fn:fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

declare %private function _:solve-naked-column-subsets($board, $n)
{
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $c1 in $ns, $c2 in $ns
    return
      let $block := fn:filter($un, function($i){$i?c1 eq $c1 and $i?c2 eq $c2})
      return
        _:paired-possibles($block, $n)
  )
  => fn:fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

declare %private function _:solve-naked-row-subsets($board, $n)
{
  (
    let $un := _:get-unsolved($board)
    let $ns := 1 to 3
    for $r1 in $ns, $r2 in $ns
    return
      let $block := fn:filter($un, function($i){$i?r1 eq $r1 and $i?r2 eq $r2})
      return
        _:paired-possibles($block, $n)
  )
  => fn:fold-left($board, function($acc, $i){
    b:remove-from-possible($acc, $i?c1, $i?r1, $i?c2, $i?r2, $i?v)
  })
};

declare %private function _:paired-possibles($cells, $n)
{
  for $a in _:permutations($cells, $n)
  let $ms := $a?*
  let $ps := $ms?p => distinct-values()
  where 
    count($ps) eq $n
  let $ns := fn:filter($cells, function($i){
    $i?p = $ps and
    (every $x in $ms satisfies deep-equal($i, $x) => not())
  })
  for $p in $ps
  for $n in $ns
  return
    map:put($n, 'v', $p)
};

declare %private function _:paired-hidden($cells, $n)
{
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
 :)
declare %private function _:solve-block-column($board)
{
  (
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
      }
  ) 
  => fn:fold-left($board, function($acc, $m){
    (
      let $c1 := $m('c1'), $r1 := $m('r1'), $c2 := $m('c2'), $p := $m('p')
      for $r1 in (1 to 3)[. ne $r1]
      for $r2 in 1 to 3
      return
        map{
          'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'p' : $p
        }
    ) 
    => fn:fold-left($acc, function($acc1, $val){
      b:remove-from-possible($acc1, $val?c1, $val?r1, $val?c2, $val?r2, $val?p)
    })
  })
};
declare %private function _:solve-block-row($board)
{
  (
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
      }
  )
  => fn:fold-left($board, function($acc, $m){
    (
      let $c1 := $m('c1'), $r1 := $m('r1'), $r2 := $m('r2'), $p := $m('p')
      for $c1 in (1 to 3)[. ne $c1]
      for $c2 in 1 to 3
      return
        map{
           'c1' : $c1, 'c2' : $c2, 'r1' : $r1, 'r2' : $r2, 'p' : $p
         }
    ) => fn:fold-left($acc, function($acc1, $val){
      b:remove-from-possible($acc1, $val?c1, $val?r1, $val?c2, $val?r2, $val?p)
    })
  })
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
      [$p, $u]
  let $ur :=
    if (exists($uc)) then () 
    else
      for $u in $un
      let $r1 := $u?r1, $r2 := $u?r2, $ps := $u?p
      for $p in $ps
      group by 
        $p, $r1, $r2
      where
        count($u) eq 1
      return
        [$p, $u]
  let $ub :=
    if (exists(($uc, $ur))) then () 
    else
      for $u in $un
      let $c1 := $u?c1, $r1 := $u?r1, $ps := $u?p
      for $p in $ps
      group by 
        $p, $c1, $r1
      where
        count($u) eq 1
      return
        [$p, $u]
  let $u := ($uc, $ur, $ub)[1]
  return
    if (empty($u)) then $board
    else
      let $v := $u(1)
      let $u := $u(2)
      return
        b:set($board, $u?c1, $u?r1, $u?c2, $u?r2, $v)
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
    )[1]
  return
    if ($s => empty()) then $board
    else
      b:set($board, $s?c1, $s?r1, $s?c2, $s?r2, $s?v)
};

declare %private function _:hidden-permutations($maps, $n)
{
  if ($n > count($maps)) then
    switch ($n)
      case 2 return 
        _:hidden_permutations_2($maps)
      case 3 return 
        _:hidden_permutations_3($maps)
      case 4 return 
        _:hidden_permutations_4($maps)
      default return ()
    else ()
};

declare %private function _:permutations($maps, $n)
{
  if ($n > count($maps)) then
    switch ($n)
      case 2 return 
        _:permutations_2($maps)
      case 3 return 
        _:permutations_3($maps)
      case 4 return 
        _:permutations_4($maps)
      default return ()
    else ()
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
                    (: trace($cnts, 'multiple solutions '), :)
                    fn:error($err:value)
                  } 
    return
      if (deep-equal($board1, $board)) then
        (: no change :)
        _:do-loop(
          $board,
          $cnts, 
          tail($funs),
          $allFuns
        )
      else
        _:do-loop(
          $board1 => _:dump($key), 
          _:incr_cnt($cnts, $key (: => trace('did ') :), $weight),
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

