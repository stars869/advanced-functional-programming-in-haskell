{-# LANGUAGE InstanceSigs #-}

-- Example: a simple evaluator

data Expr = Val Int | Div Expr Expr 

-- eval :: Expr -> Int 
-- eval (Val n) = n 
-- eval (Div x y) = eval x `div` eval y 

safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing 
safediv x y = Just (x `div` y)

-- eval :: Expr -> Maybe Int 
-- eval (Val n) = Just n 
-- eval (Div x y) = case eval x of 
--     Nothing -> Nothing 
--     Just n -> case eval y of 
--         Nothing -> Nothing 
--         Just m -> safediv n m 

-- -- use Applicative? safediv is not pure. 
-- eval :: Expr -> Maybe Int 
-- eval (Val n) = pure n 
-- eval (Div x y) = pure safediv <*> eval x <*> eval y 

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- (>>=) mx f = case mx of 
--          Nothing -> Nothing
--          Just x -> f x  

eval :: Expr -> Maybe Int 
eval (Val n) = Just n
eval (Div x y) = eval x >>= (\n -> 
                 eval y >>= (\m ->
                 safediv n m))

-- do Notation
eval' :: Expr -> Maybe Int 
eval' (Val n) = Just n 
eval' (Div x y) = do n <- eval' x
                     m <- eval' y 
                     safediv n m  

-- Example: Lists

-- instance Monad [] where 
--     (>>=) :: [a] -> (a -> [b]) -> [b]
--     xs >>= f = concatMap f xs
    
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do x <- xs 
                 y <- ys 
                 return (x, y)
