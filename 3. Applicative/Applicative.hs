{-# LANGUAGE InstanceSigs #-}
import Prelude (Int, (+), (^), IO, print)
import GHC.Show (Show)

-- Generalising fmap

-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c 
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d


-- Example: Maybe 
data Maybe a = Nothing | Just a 
    deriving Show

class Functor f where 
    fmap :: (a -> b) -> f a -> f b 

instance Functor Maybe where 
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap g Nothing = Nothing 
    fmap g (Just x) = Just (g x)

class Functor f => Applicative f where 
    pure :: a -> f a 
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where 
    pure :: a -> Maybe a 
    pure x = Just x

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (<*>) Nothing mx = Nothing 
    (<*>) (Just g) mx = fmap g mx 

-- Example: Lists
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs 

instance Functor [] where 
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

instance Applicative [] where 
    pure :: a -> [a]
    pure x = [x]

    (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]

-- main 
main :: IO ()
main = print (pure (+) <*> [1, 2, 3] <*> [4, 5, 6])