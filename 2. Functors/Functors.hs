{-# LANGUAGE InstanceSigs #-}
import Prelude (Int, (+), (^), IO, print)

-- Abstract programming patterns

-- inc :: [Int] -> [Int]
-- inc [] = []
-- inc (n:ns) = n+1 : inc ns

-- sqr :: [Int] -> [Int]
-- sqr [] = []
-- sqr (n:ns) = n^2 : sqr ns 

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs 

-- inc :: [Int] -> [Int]
-- inc = map (+1)

-- sqr :: [Int] -> [Int]
-- sqr = map (^2)

-- Generalising further

class Functor f where 
    fmap :: (a -> b) -> f a -> f b 

instance Functor [] where 
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

-- The Maybe functor

data Maybe a = Nothing | Just a 

instance Functor Maybe where 
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap g Nothing = Nothing 
    fmap g (Just x) = Just (g x)

-- The Tree functor

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)

t :: Tree Int 
t = Node (Leaf 1) (Leaf 2)

instance Functor Tree where 
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)
    
inc :: Functor f => f Int -> f Int 
inc = fmap (+1)

-- main

main :: IO ()
main = print (inc [1,2,3])