module Polynomials where
import Expressions

singleFactor 0 a = Const a
singleFactor n a = Mult (Const a) (Exp (Iden) (Const n))

genPolynomialExpr :: (Eq a, Floating a) => [a] -> Expr a
genPolynomialExpr xs = Sums $ gen len xs
  where
    len = fromIntegral $ (length xs) - 1
    gen n (a:[]) = [singleFactor n a]
    gen n (a:as) = (singleFactor n a) : (gen (n-1) as)