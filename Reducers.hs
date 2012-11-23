module Reducers where
import Expressions

--can shorten
shorten (Sums xs) = Sums $ shorten' xs
  where shorten' [] = []
        shorten' (x:xs) = case x of
          (Const 0) -> shorten' xs
          otherwise -> x:(shorten' xs)

shorten (Mult (Const a) (Const b)) = Const (a * b)
shorten (Mult (Const 0) _) = Const 0
shorten (Mult _ (Const 0)) = Const 0
shorten (Mult (Const 1) e) = e
shorten (Mult e (Const 1)) = e

shorten (Div (Const a) (Const b)) = Const (a / b)
shorten (Div eA (Const 1)) = eA

shorten (Exp (Const a) (Const b)) = Const (a ** b)
shorten (Exp eA (Const 1)) = eA
shorten (Exp (Const 1) _) = Const 1

shorten (Log _ (Const 1)) = Const 0

shorten s = s

--guards
flatten (Iden) = Iden
flatten con@(Const _) = con
--backtrack
flatten (Sums xs) = shorten $ Sums $ flt xs
  where flt [] = []
        flt (x:xs) = (flatten x):(flt xs)
flatten (Mult eA eB) = shorten $ Mult (flatten eA) (flatten eB)
flatten (Div eA eB) = shorten $ Div (flatten eA) (flatten eB)
flatten (Exp eA eB) = shorten $ Exp (flatten eA) (flatten eB)
flatten (Log eA eB) = shorten $ Log (flatten eA) (flatten eB)