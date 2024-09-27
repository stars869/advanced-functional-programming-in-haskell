import Prelude ()


(++) :: [a] -> [a] -> [a]
[] ++ ys = ys 
(x:xs) ++ ys = x:(xs ++ ys)

-- time complexity O(n^2)
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

-- time complexity O(n)
reverse'' :: [a] -> [a]
reverse'' xs = reverse' xs []

