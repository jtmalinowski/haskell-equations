module Derivatives where
import Expressions

--differentiate

--(f+g)' = f' + g'
diff (Sum eA eB) = Sum (diff eA) (diff eB)
diff (Sums xs) = Sums $ df xs
  where df [] = [Const 0]
        df (x:xs) = (diff x):(df xs)

--(f+g)' = f' - g'
diff (Subtr eA eB) = Subtr (diff eA) (diff eB)

--(f*g)' = f'g + g'f
diff (Mult eA eB) = Sum (Mult (diff eA) eB) (Mult (diff eB) eA)

--(f/g)' = (f'g - g'f) / (g*g)
diff (Div eA eB) = Div (Subtr (Mult (diff eA) eB) (Mult (diff eB) eA)) (Mult eB eB)

--(f^g)' = f^g( (f'g)/f + g'(ln f) )
diff (Exp eA eB) = Mult (Exp eA eB) (Sum (Div (Mult (diff eA) eB) eA) (Mult (diff eB) (Log (Const 2) (eA))))

--ln f = f' / f
diff (Log (Const _) (Const _)) = (Const 0)
diff (Log (Const 2) eA) = Div (diff eA) eA

--(log[f](g))' = (ln(g) / ln(f))'
diff (Log eA eB) = diff (Div (Log (Const 2) eB) (Log (Const 2) eA))

diff (Iden) = Const 1
diff (Const _) = Const 0