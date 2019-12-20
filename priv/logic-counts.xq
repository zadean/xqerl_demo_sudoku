declare function local:difficulty($p)
{
  if ($p/@coloring or
      $p/@forcing_chain) then 'Expert'
  else if ($p/@hidden_4 or
           $p/@hidden_3 or
           $p/@naked_4 or
           $p/@xy_wing or
           $p/@x_wing or
           $p/@xyz_wing) then 'Hard'
  else if ($p/@hidden_2 or
           $p/@naked_3 or
           $p/@block or
           $p/@naked_2) then 'Medium'
  else 'Easy'
};

(
  for $p in collection('http://xqerl.org/sudoku/puzzles/run8')/s/p
  group by
    $l := local:difficulty($p)
  order by 
    count($p)
  return
    map {$l : count($p)}
)
=> map:merge()
=> erlang:item-to-term()
