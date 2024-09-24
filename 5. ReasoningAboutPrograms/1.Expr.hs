
data Expr a = Var a
            | Val Int
            | Add (Expr a) (Expr a)


x_plus_1 :: Expr Char
x_plus_1 = Add (Var 'x') (Val 1)


x_plus_1' :: Expr String
x_plus_1' = Add (Var "x") (Val 1)


instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a) = Var (f a)
  fmap f (Val n) = Val n
  fmap f (Add x y) = Add (fmap f x) (fmap f y)
  

instance Applicative Expr where 
  pure :: a -> Expr a
  pure = Var 

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (<*>) (Var f) (Var a) = Var (f a)
  (<*>) (Var f) (Val n) = Val n 
  (<*>) (Var f) (Add x y) = Add (fmap f x) (fmap f y)
  -- Not sure about other cases
  (<*>) _ _ = Val 0 


instance Monad Expr where 
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var v) >>= f = f v 
  (Val n) >>= f = Val n 
  (Add x y) >>= f = Add (x >>= f) (y >>= f)


f :: Char -> Expr a 
f 'x' = Add (Val 1) (Val 2)
f 'y' = Val 3 


example :: Expr b
example = Add (Var 'x') (Var 'y') >>= f 