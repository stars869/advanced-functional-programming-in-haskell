

-- Source language 

data Expr = Val Int | Add Expr Expr 


-- Semantics

eval :: Expr -> Int 
eval (Val v) = v 
eval (Add x y) = eval x + eval y 


-- Virtual machine 

type Stack = [Int]
type Code = [Op]

data Op = PUSH Int | ADD 
    deriving Show


-- Semantics
exec :: Code -> Stack -> Stack 
exec [] s = s 
exec (PUSH n : c) s = exec c (n:s)
exec (ADD : c) (m:n:s) = exec c (m+n : s)


-- Compiler
comp :: Expr -> Code 
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]


-- Example 
e :: Expr
e = Add (Add (Val 2) (Val 3)) (Val 4)


-- Compiler correctness
-- exec (comp e) [] = [eval e]  
-- easier to prove: exec (comp e) s = (eval e):s

-- Induciton on e:
-- Base case: e = Val n 
-- exec (comp (Val n)) s = (eval (Val n)):s
--       exec [PUSH n] s = n:s
--                   n:s = n:s

-- 'Add' case: e = Add x y
--            exec (comp (Add x y)) s = (eval (Add x y)):s
-- exec (comp x ++ comp y ++ [ADD]) s = (eval x + eval y):s
-- Apply distributivity lemma: exec (c ++ d) s = exec d (exec c s)
-- exec [ADD] (exec (comp y) (exec (comp x) s)) = (eval x + eval y):s
-- Apply Induction hypothesis:
-- exec [ADD] (eval y : eval x : s) = (eval x + eval y):s
--    exec [] ((eval y + eval x):s) = (eval x + eval y):s
--              (eval y + eval x):s = (eval y + eval x):s


-- Rewrite comp function
comp' :: Expr -> Code -> Code 
comp' (Val n) c = PUSH n :c 
comp' (Add x y) c = comp' x (comp' y (ADD:c))

comp'' :: Expr -> Code
comp'' e = comp' e []


-- new proof:
-- exec (comp' e c) s = exec c (eval e : s)

-- Base case:
-- exec (comp' (Val n) c) s = exec c (eval e : s)
--      exec (PUSH n : c) s = exec c (eval Val n : s)
--             exec c (n:s) = exec c (n:s)

-- Inductive case:
--          exec (comp' (Add x y) c) s = exec c (eval (Add x y) : s)
--  exec (comp' x (comp' y (ADD:c))) s = exec c (eval (Add x y) : s)
-- Apply Induction hypothesis
-- exec (comp' y (ADD:c)) (eval x : s) = exec c (eval (Add x y) : s)
--  exec (ADD:c) (eval y : eval x : s) = exec c (eval (Add x y) : s)
--       exec c ((eval x + eval y): s) = exec c (eval (Add x y) : s)
--       exec c ((eval x + eval y): s) = exec c ((eval x + eval y) : s)


-- Benifits after rewirting
-- 22 steps -> 8 steps
-- 2 lemmas -> 0 lemmas
-- use of ++ -> vanished
-- stack underflow -> vanished
