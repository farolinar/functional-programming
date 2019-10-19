-- Arithmetic Expressions
-- Studying my friend's trial for tree
-- Studied for quiz
data Expr = C Float
          | Add Expr Expr 
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

evaluate           :: Expr -> Float
evaluate (C x)      = x
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2
evaluate (Div e1 e2) = evaluate e1 / evaluate e2

-- Tree Data Structure
data MyTree a = MyLeaf a
              | MyBranch (MyTree a) (MyTree a)
data SimpleTree = SLeaf
              | SBranch (SimpleTree) (SimpleTree)
data FancyTree a b = FLeaf a
              | FBranch b (FancyTree a b) (FancyTree a b)

-- Exercise 7.1
foldTree :: (a -> a -> a) -> (b -> a) -> MyTree b -> a
foldTree combine leafFn (MyLeaf x) = leafFn x
foldTree combine leafFn (MyBranch t1 t2) = combine (foldTree combine leafFn t1)(foldTree combine leafFn t2)

fringe :: MyTree a -> [a] 
fringe t = foldTree (++) leafFn t
  where leafFn x = [x]

treeSize t = foldTree (+) (const 1) t

treeHeight t = foldTree combine (const 0) t
  where combine x y = 1 + max x y

tree1 = MyBranch (MyBranch (MyLeaf 1) (MyLeaf 2)) (MyBranch (MyLeaf 3) (MyLeaf 4))

-- Exercise 7.2
data InternalTree a = ILeaf
                    | IBranch a (InternalTree a) (InternalTree a)
                    deriving Show

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree 0 t = ILeaf
takeTree n ILeaf = ILeaf
takeTree n (IBranch a x y) = IBranch a (takeTree (n-1) x) (takeTree (n-1) y)

t = IBranch 2 (IBranch 1 (IBranch 3 ILeaf ILeaf) ILeaf) (IBranch 1 ILeaf ILeaf)

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile f ILeaf = ILeaf
takeTreeWhile f (IBranch a x y) = if (f a)
                                  then IBranch a (takeTreeWhile f x) (takeTreeWhile f y)
                                  else ILeaf
