import module namespace s = 'http://xqerl.org/sudoku/solver' at 'sudoku-solve.xqm';
import module namespace b = 'http://xqerl.org/sudoku/board' at 'sudoku-boards.xqm';
import module namespace c = 'http://xqerl.org/sudoku/create' at 'sudoku-create.xqm';

(: X-CYCLE on 1 (Discontinuous Alternating Nice Loop, Double weak-link) :)
(: '924085601000400280000000050301204005000051000000009106030000010012008060009140507' :) 
(: X-CYCLE on 1 (Discontinuous Alternating Nice Loop, Double strong-link) :) 
(: '804537000023614085605982034000105870500708306080203450200859003050371208008426507' :)
(: X-CYCLE (Alternating Inference Chain) , has XY-chain later :)
'024100670060070410700964020246591387135487296879623154400009760350716940697040031'

(: '003000700090000040000978000802001300600040007409300602000763000080000000004000100' :)
=> b:board-from-string()
=> s:solve()

(: => array:get(3) => b:board-string() :)

 (: b:new-random() => b:board-string() => b:translate()
=> c:backtrack()
=> b:board-from-string()
=> s:solve() :)
