-- any imports go here
import Data.List


{-Begin Question 1.1-}
digits :: Int -> [Int]
digits 0 = []
digits x
  = digits (x `div` 10) ++ [x `mod` 10]
{-End Question 1.1-}

{-Begin Question 1.2-}
isPar :: Int -> Bool
isPar 0 = False
isPar x
  | b `mod` a == 0 && allDifferent ls && notZero ls = True
  | otherwise = False
  where
    ls = digits x
    a = (ls !! 0)*10 + (ls !! 1)
    b = (ls !! 2)*10 + (ls !! 3)

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

notZero::[Int]->Bool
notZero xs = 0 `notElem` xs


pars :: [Int]
pars = parConsumer producer

producer :: [ Int ]
producer = [ 1000..9999 ]

parConsumer :: [ Int ] -> [ Int ]
parConsumer
  = filter isPar
{-End Question 1.2-}

{-Begin Question 1.3-}
isParty :: (Int, Int) -> Bool
isParty (x,0) = False
isParty (x,y)
  = allDifferent ls
  where
    ls = digits x ++ digits y


--partys :: [(Int, Int)]
partys = partyConsumer [(x,y) | x <- pars, y <- pars]
  

partyConsumer :: [(Int,Int)]->[(Int,Int)]
partyConsumer = filter isParty


{-End Question 1.3-}

-- any main functions for testing goes here

main::IO()
main = putStrLn (show (partys))