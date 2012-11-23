module Derivatives where
import Prelude hiding(subtract)
import Expressions

--differentiate

--(f+g)' = f' + g'
diff (Sums xs) = Sums $ foldl (\acc x -> (diff x):acc) [] xs

--(f*g)' = f'g + g'f
diff (Mult eA eB) = add (Mult (diff eA) eB) (Mult (diff eB) eA)

--(f/g)' = (f'g - g'f) / (g*g)
diff (Div eA eB) = Div (subtract (Mult (diff eA) eB) (Mult (diff eB) eA)) (Mult eB eB)

--(f^g)' = f^g( (f'g)/f + g'(ln f) )
diff (Exp eA eB) = Mult (Exp eA eB) (add (Div (Mult (diff eA) eB) eA) (Mult (diff eB) (Log (Const 2) (eA))))

--ln f = f' / f
diff (Log (Const _) (Const _)) = (Const 0)
diff (Log (Const 2) eA) = Div (diff eA) eA

--(log[f](g))' = (ln(g) / ln(f))'
diff (Log eA eB) = diff (Div (Log (Const 2) eB) (Log (Const 2) eA))

diff (Iden) = Const 1
diff (Const _) = Const 0