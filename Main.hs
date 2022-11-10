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

-- Runs Extremely slow on repl but faster on alternative Haskell compilers
possibles :: [([Int],[Int])]
possibles = splitter z
  where
  z = permutations[1,2,3,4,5,6,7,8,9]

splitter::[[Int]]->[([Int],[Int])]
splitter [x] = splits x
splitter (x:xs) = splits x ++ splitter xs
{-End Question 2.2-}

{-Begin Question 2.3-}

tupProduct:: ([Int],[Int])->Int
tupProduct ([],[]) = 0
tupProduct ([x],[]) = 0
tupProduct (xs, ys) = number xs * number ys

isPalindrome::Int->Bool
isPalindrome x
  | show(x) == reverse(show(x)) = True
  | otherwise = False

start4::Int->Bool
start4 0 = False
start4 x
  | (digits x) !! 0 == 4 =True
  | otherwise = False

digits :: Int -> [Int]
digits 0 = []
digits x
  = digits (x `div` 10) ++ [x `mod` 10]

smaller::([Int],[Int])->[Int]
smaller (xs,ys)
  | number xs > number ys = ys
  | number xs < number ys = xs
smaller3::([Int],[Int])->Bool
smaller3 ([],[])=False
smaller3 x 
  | (number(smaller x)) `mod` 10 == 3 = True
  | otherwise = False
isAcceptable :: ([Int],[Int]) -> Bool
isAcceptable x
  | smaller3 x && start4 y && isPalindrome y = True
  | otherwise = False
  where 
  y = tupProduct x

acceptables :: [([Int],[Int])]
acceptables = filter isAcceptable possibles
{-End Question 2.3-}

-- any main functions for testing goes here
main = putStrLn(show(length(possibles)))