
-- Induction on Numbers

data Nat = Zero | Succ Nat 

-- Zero
-- Succ Zero
-- Succ (Succ Zero)

-- inf :: Nat
-- inf = Succ inf

-- Induction

--  p(Zero); for all k, p(k) => p(Succ k)
--  -------------------------------------
--           for all n, P(n)


-- Example 1

--given
add :: Nat -> Nat -> Nat 
add Zero m = m
add (Succ n) m = Succ (add n m)

-- show that
-- add n Zero = n

-- Proof by induction
-- P(n) === add n Zero = n

-- base case:
-- P(Zero) === add Zero Zero = Zero
--                      Zero = Zero

-- Induction hypothesis: P(m) === add m Zero = m
-- P(Succ m) === add (Succ m) Zero = Succ m
--               Succ (add m Zero) = Succ m
-- apply Induction hypothesis P(m) on the left side:
--                          Succ m = Succ m


-- Example 2
-- show that 
-- add x (add y z) = add (add x y) z

-- Proof by induction
-- P(n) === add n (add y z) = add (add n y) z

-- base case: 
-- P(Zero) === add Zero (add y z) = add (add Zero y) z
--                        add y z = add y z

-- Induction hypothesis: P(m) === add m (add y z) = add (add m y) z
-- P(Succ m) === add (Succ m) (add y z) = add (add (Succ m) y) z
--               Succ (add m (add y z)) = add (Succ (add m y)) z
--                                      = Succ (add (add m y) z)
--  apply Induction hypothesis P(m) on the left side:
--               Succ (add (add m y) z) = Succ (add (add m y) z)


-- Induction on Lists

-- P([]); for all k, for all ks, P(ks) => P(k:ks)
-- ----------------------------------------------
--         for all xs, P(xs)


-- Example:
-- given
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- show that
-- rev (rev xs) = xs

-- proof by induction
-- P(xs) === rev (rev xs) = xs

-- base case:
-- P([]) === rev (rev []) = []
--                     [] = []

-- Induction hypothesis: P(ks) === rev (rev ks) = ks
-- P(k:ks) === rev (rev (k:ks)) = k:ks
--          rev (rev ks ++ [k]) = k:ks
-- apply distributivity lemma： rev (xs ++ ys) = rev ys ++ rev xs
--      rev [k] ++ rev (rev ks) = k:ks
-- apply singleton list lemma: rev [x] = [x]
--          [k] ++ rev (rev ks) = k:ks
-- apply Induction hypothesis P(ks):
--                    [k] ++ ks = k:ks
--                         k:ks = k:ks

