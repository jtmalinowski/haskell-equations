module Expressions where
import Prelude hiding(sum)

data Expr a =
  Sums [Expr a] |
  Mult (Expr a) (Expr a) |
  Div (Expr a) (Expr a) |
  Exp (Expr a) (Expr a) |
  Log (Expr a) (Expr a) |
  Iden |
  Const a

showRawSums [] = ""
showRawSums (expr:[]) = show expr
showRawSums (expr:exprs) = (show expr) ++ " + " ++ (showRawSums exprs)

instance (Show a) => Show (Expr a) where
  show (Sums s) =      "( " ++ showRawSums s  ++ " )"
  show (Mult eA eB) =   "(" ++ (show eA)  ++ " * " ++ (show eB) ++ ")"
  show (Div eA eB) =    "(" ++ (show eA)  ++ " / " ++ (show eB) ++ ")"
  show (Exp eA eB) =    "(" ++ (show eA)  ++ " ^ " ++ (show eB) ++ ")"
  show (Log eA eB) = "log[" ++ (show eA)  ++ "]("  ++ (show eB) ++ ")"
  show (Iden) = "x"
  show (Const a) = show a


add eA eB = Sums [eA, eB]
subtract eA eB = Sums [eA, (Mult (Const (-1)) eB)]

run (Sums xs) x = foldl (\acc f -> acc + (run f x)) 0.0 xs
run (Mult eA eB) x = (run eA x) * (run eB x)
run (Div eA eB) x = (run eA x) / (run eB x)
run (Exp eA eB) x = (run eA x) ** (run eB x)
run (Log eA eB) x = logBase (run eA x) (run eB x)
run (Iden) x = x
run (Const a) _ = a

runShow e x = show $ run e x