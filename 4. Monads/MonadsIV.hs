import Prelude hiding (mapM, concat)
import Data.Char (isDigit, digitToInt)

-- Generics

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] = return []
mapM f (x:xs) = do y <- f x 
                   ys <- mapM f xs 
                   return (y:ys)

-- Example: Convert char to int
conv :: Char -> Maybe Int 
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing 

-- > mapM conv "1234"

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx 
              x <- mx
              return x  

-- > join [[1,2],[3,4],[5,6]]
-- > join (Just (Just 1))


-- Monad laws 

-- return x >>= f = f x
-- mx >>= return = mx
-- (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))


-- Effectful Programming

-- Type              Effect
-- ---------------------------------
-- a -> Maybe a      Exceptions
-- a -> [b]          Non-determinism
-- a -> ST b         Internal state
-- a -> IO b         Input/Output


-- What's the point of monads?
-- 1. pure programming with effects
-- 2. effects are explicit in types
-- 3. generalise functions to any effect