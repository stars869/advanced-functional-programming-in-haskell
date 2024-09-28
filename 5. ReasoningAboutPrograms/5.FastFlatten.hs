

data Tree = Leaf Int | Node Tree Tree


flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r 


flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ys = n:ys
flatten' (Node l r) ys = flatten' l (flatten' r ys)

flatten'' :: Tree -> [Int]
flatten'' xs = flatten' xs []

