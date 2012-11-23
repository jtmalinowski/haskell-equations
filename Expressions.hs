module Expressions where
import Prelude hiding(sum)

data Expr a =
  Sum (Expr a) (Expr a) |
  Sums [Expr a] |
  Subtr (Expr a) (Expr a) |
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
  show (Sum eA eB) =    "(" ++ (show eA)  ++ " + " ++ (show eB) ++ ")"
  show (Sums s) =      "( " ++ showRawSums s  ++ " )"
  show (Subtr eA eB) =  "(" ++ (show eA)  ++ " - " ++ (show eB) ++ ")"
  show (Mult eA eB) =   "(" ++ (show eA)  ++ " * " ++ (show eB) ++ ")"
  show (Div eA eB) =    "(" ++ (show eA)  ++ " / " ++ (show eB) ++ ")"
  show (Exp eA eB) =    "(" ++ (show eA)  ++ " ^ " ++ (show eB) ++ ")"
  show (Log eA eB) = "log[" ++ (show eA)  ++ "]("  ++ (show eB) ++ ")"
  show (Iden) = "x"
  show (Const a) = show a

run (Sum eA eB) x = (run eA x) + (run eB x)
run (Subtr eA eB) x = (run eA x) - (run eB x)
run (Mult eA eB) x = (run eA x) * (run eB x)
run (Div eA eB) x = (run eA x) / (run eB x)
run (Exp eA eB) x = (run eA x) ** (run eB x)
run (Log eA eB) x = logBase (run eA x) (run eB x)
run (Iden) x = x
run (Const a) _ = a

runShow e x = show $ run e x