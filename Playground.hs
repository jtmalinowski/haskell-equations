import Expressions
import Polynomials
import Derivatives
import Reducers

expr1 = Sums [(Iden), (Iden), (Const 1)]
showExpr1 = runShow expr1

--    2x + 1
--y = ------
--    2x - 1
expr2 = Div
  (Sums [(Mult (Iden) (Const 2)), (Const 1)])
  (Sums [(Mult (Iden) (Const 2)), (Const (-1))])
showExpr2 = runShow expr2