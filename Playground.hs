import Expressions
import Polynomials
import Derivatives
import Reducers

expr1 = Sum (Sum (Iden) (Iden)) (Const 1)
showExpr1 = runShow expr1

--    2x + 1
--y = ------
--    2x - 1
expr2 = Div
  (Sum (Mult (Iden) (Const 2)) (Const 1))
  (Sum (Mult (Iden) (Const 2)) (Const (-1)))
showExpr2 = runShow expr2