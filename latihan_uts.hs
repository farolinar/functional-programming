import Data.List

data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr | Expr :/ Expr
    | V String | Let String Expr Expr
    deriving Show

subst :: String -> Expr -> Expr -> Expr
subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c) = (C c)
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2)


evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2

evaluate (Let v e0 e1) = evaluate (subst v e0 e1)

mapExpr :: (Expr -> Expr) -> [Expr] -> [Expr]
mapExpr f [] = []
mapExpr f (x:xs) = f x : (mapExpr f xs)

foldExpr :: (Expr -> Expr -> Expr) -> (Expr) -> [Expr] -> Expr
foldExpr op init_ [] = init_
foldExpr op init_ (x:xs) = x `op` (foldExpr op init_ xs)

--atau menggunakan foldl
--foldExpr op init_ [] = init_
--foldExpr op init_ (x:xs) = foldExpr op (init_ `op` x) xs

-- evaluate di kelas
--evaluate (Let "y" (C 9) (Let "x" (C 5) (V "x" :+ V "y" :+ (C 7))))
-- evaluate (subst "y" (C 9) (Let "x" (C 5) (V "x" :+ V "y" :+ (C 7))))
-- TAMBAHAN YANG TIDAK DIJELASKAN DI KELAS:
-- evaluate (Let "x" (C 5) (subst "y" (C 9)(V "x" :+ V "y" :+ (C 7))))
--evaluate (Let "x" (C 5) (subst "y" (C 9) (V "x") + subst (C 9) (V "y") + subst (C 9) (C 7)))
-- evaluate (Let "x" (C 5) (V "x" :+ (C 9) :+ (C 7)))
--  evaluate (subst "x" (C 5) (V "x" :+ (C 9) :+ (C 7))
-- evaluate ((C 5) :+ (C 9) :+ (C 7))
-- 21.0

-- evaluate menggunakan fold
--data Expr = Float | Float :+ Float | Float :- Float | Float :* Float | Float :/ Float | V String | Let String Expr Expr deriving Show

--evaluateFold :: Expr -> Float
--evaluateFold (n1 :+ n2) = foldl (+) 0 ([n1] ++ [n2])
--evaluateFold (n1 :- n2) = foldr (-) 0 ([n1] ++ [n2])
--evaluateFold (n1 :* n2) = foldl (*) 1 ([n1] ++ [n2])
--evaluateFold (n1 :/ n2) = foldr (/) 1 ([n1] ++ [n2])
