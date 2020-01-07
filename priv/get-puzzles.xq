declare function local:difficulty($p)
{
  if ($p/@wxyz_wing or
      $p/@coloring or
      $p/@forcing_chain) then 'Expert'
  else if ($p/@hidden_4 or
           $p/@hidden_3 or
           $p/@naked_4 or
           $p/@chain_1 or
           $p/@x_cycle or
           $p/@xy_wing or
           $p/@x_wing or
           $p/@xyz_wing) then 'Hard'
  else if ($p/@hidden_2 or
           $p/@naked_3 or
           $p/@block or
           $p/@naked_2) then 'Medium'
  else 'Easy'
};

declare function local:puzzle-map($puzz)
{
  let $sol := $puzz/@solution => string()
  let $ps := (
    for $p in $puzz/p
    let $dif := local:difficulty($p)
    let $hnt := $p/data() => string()
    return
      map{$dif : $hnt}
  ) => map:merge(map{'duplicates' : 'combine'})
  return
    [$sol , $ps]
};

(
  for $puzz in collection('http://xqerl.org/sudoku/puzzles/run8')/s
  return
    local:puzzle-map($puzz)
) 
(: => erlang:item-to-term() :)