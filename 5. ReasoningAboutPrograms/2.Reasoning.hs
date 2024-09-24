
-- Equational Reasoning
  -- x * y = y * x
  -- x + (y + z) = (x + y) + z
  -- x * (y + z) = x * y + x * z
  -- (x + y) * z = x * z + y * z

  -- (x + a) * (x + b) ==> x * x + (a + b) * x + a * b


-- Reasoning About Haskell

double :: Int -> Int 
double x = x + x

isZero :: Int -> Bool
isZero 0 = True 
isZero n = False 

-- non-overlapping patterns
-- isZero' :: Int -> Bool
-- isZero' 0 = True 
-- isZero' n | n != 0 = False 

