import Data.List (transpose)

-- Basic declarations

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

boxsize :: Int
boxsize = 3

-- Examples 

-- Solvable only using the basic rules:
easy         :: Grid
easy         = ["2....1.38",
                "........5",
                ".7...6...",
                ".......13",
                ".981..257",
                "31....8..",
                "9..8...2.",
                ".5..69784",
                "4..25...."]

-- First gentle example from sudoku.org.uk:
gentle       :: Grid
gentle       = [".1.42...5",
                "..2.71.39",
                ".......4.",
                "2.71....6",
                "....4....",
                "6....74.3",
                ".7.......",
                "12.73.5..",
                "3...82.7."]

-- First diabolical example:
diabolical   :: Grid
diabolical   = [".9.7..86.",
                ".31..5.2.",
                "8.6......",
                "..7.5...6",
                "...3.7...",
                "5...1.7..",
                "......1.9",
                ".2.6..35.",
                ".54..8.7."]

-- First "unsolvable" (requires backtracking) example:
unsolvable   :: Grid
unsolvable   = ["1..9.7..3",
                ".8.....7.",
                "..9...6..",
                "..72.94..",
                "41.....95",
                "..85.43..",
                "..3...7..",
                ".5.....4.",
                "2..8.6..9"]

-- Minimal sized grid (17 values) with a unique solution:
minimal     :: Grid
minimal      = [".98......",
                "....7....",
                "....15...",
                "1........",
                "...2....9",
                "...9.6.82",
                ".......3.",
                "5.1......",
                "...4...2."]

-- Empty grid:
blank :: Grid
blank = replicate n (replicate n '.')
        where n = boxsize ^ 2

-- Helper functions

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxs :: Matrix a -> [Row a]
boxs = map (concat . transpose) . chunk boxsize . concat . map transpose . chunk boxsize

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs



-- A basic solver
type Choices = [Value]

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices

choices :: Matrix Value -> Matrix Choices
choices = map (map choice)
    where
        choice :: Value -> Choices
        choice v = if v == '.' then ['1'..'9'] else [v]

collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)
    where
        cp :: [[a]] -> [[a]]
        cp [] = [[]]
        cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

-- pruning
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
    where
        pruneBy f = f . map pruneRow . f

        pruneRow :: Row Choices -> Row Choices
        pruneRow r = map pruneCs r
                where
                    choicesMade = concat (filter (\cs -> length cs == 1) r) :: Choices
                    pruneCs :: Choices -> Choices
                    pruneCs cs = if length cs == 1 then cs else filter (`notElem` choicesMade) cs

solve2 :: Grid -> [Grid]
solve2 = filter valid . collapse . prune . choices

-- repeat pruning until we got a fixpoint
solve3 :: Grid -> [Grid]
solve3 = filter valid . collapse . fix prune . choices

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
    where x' = f x

-- More optimization

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool 
safe cm = all consistent (rows cm) &&
          all consistent (cols cm) &&
          all consistent (boxs cm) 
        where 
            consistent :: Row Choices -> Bool 
            consistent = nodups . concat . filter single 

single :: Choices -> Bool 
single [_] = True 
single _ = False 

blocked :: Matrix Choices -> Bool 
blocked m = void m || not (safe m)

solve4 :: Grid -> [Grid]
solve4 = search . prune . choices 

search :: Matrix Choices -> [Grid]
search m | blocked m = []
         | all (all single) m = collapse m 
         | otherwise = [g | m' <- expand m, g <- search (prune m')]
    where 
        expand :: Matrix Choices -> [Matrix Choices]
        expand = map (chunk (boxsize ^ 2)) . expandRow . concat

        expandRow :: Row Choices -> [Row Choices]
        expandRow [] = [[]]
        expandRow ([c]:css) = [[c]:r | r <- expandRow css]
        expandRow (cs:css) = [[c]:css | c <- cs]

-- main

main :: IO ()
main = print (solve4 gentle)