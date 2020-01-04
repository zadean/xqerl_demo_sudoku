for $p in collection('http://xqerl.org/sudoku/puzzles/run8')/s/p
for $a in $p/@*
let $v := $a
let $n := $a/name() => string()
group by
  $n
return
  $n || ' ' || sum($v)
