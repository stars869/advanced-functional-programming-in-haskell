
-- Example: State

type State = Int

-- type ST a = State -> (a, State)

newtype ST a = S (State -> (a, State)) 

app :: ST a -> State -> (a, State)
app (S st) s = st s 


instance Functor ST where 
    fmap :: (a -> b) -> ST a -> ST b
    fmap f (S st) = S st' where 
        st' s = (f v, s') where 
            (v, s') = st s 

instance Applicative ST where 
    pure :: a -> ST a
    pure v = S (\s -> (v, s))

    (<*>) :: ST (a -> b) -> ST a -> ST b
    st <*> st' = S (\s -> 
        let (x, s') = app st' s 
        in let (f, s'') = app st s' 
        in (f x, s''))

instance Monad ST where 
    (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> 
        let (x, s') = app st s
        in app (f x) s')

-- Example: relabelling trees

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show 

t :: Tree Char 
t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- rlabel :: Tree a -> Int -> (Tree Int, Int)
-- rlabel (Leaf x) n = (Leaf n, n + 1)
-- rlabel (Node l r) n = (Node l' r', n'')
--     where  
--         (l', n') = rlabel l n 
--         (r', n'') = rlabel r n'

fresh :: ST Int 
fresh = S $ (\n -> (n, n + 1))

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf x) = do n <- fresh 
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l 
                       r' <- mlabel r
                       return (Node l' r') 
                       
rlabel :: Tree a -> Tree Int 
rlabel t = fst (app (mlabel t) 0)

-- main 
main :: IO ()
main = print (rlabel t)