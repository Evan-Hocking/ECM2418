-- any imports go here
import Data.List
{-Begin Question 2.1-}
number :: [Int] -> Int
number = foldl addDigit 0
   where addDigit no x = 10*no + x
{-End Question 2.1-}

{-Begin Question 2.2-}
splits :: [a] -> [([a],[a])]
splits [] = []
splits [x]=[]
splits (x:xs) = ([x], xs) : map func (splits xs)
    where func (a, b) = (x:a,b)

--NOTE Ran slow in repl but worked in alternative Haskell Compilers
possibles :: [([Int],[Int])]
possibles = splitter z
  where
  z = permutations[1,2,3,4,5,6,7,8,9]

splitter::[[Int]]->[([Int],[Int])]
splitter [x] = splits x
splitter (x:xs) = splits x ++ splitter xs
{-End Question 2.2-}

{-Begin Question 2.3-}
--isAcceptable :: ([Int],[Int]) -> Bool

--acceptables :: [([Int],[Int])]

{-End Question 2.3-}

-- any main functions for testing goes here
main = putStrLn(show(length(possibles)))