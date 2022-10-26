-- any imports go here

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
  | b `mod` a == 0 && con == True = True
  | otherwise = False
  where
    ls = digits x
    a = (ls !! 0)*10 + (ls !! 1)
    b = (ls !! 2)*10 + (ls !! 3)
    con = allUnique ls

memberSet::Eq p => p -> [p] -> Bool
memberSet e []
  =False
memberSet e (x:xs)
  |e==x = True
  |otherwise = memberSet e xs

allUnique:: [Int]->Bool
allUnique [x] = True
allUnique (x:xs)
  | memberSet x xs == True = False
  | otherwise = allUnique xs

--pars :: [Int]

{-End Question 1.2-}

{-Begin Question 1.3-}
--isParty :: (Int, Int) -> Bool

--partys :: [(Int, Int)]

{-End Question 1.3-}

-- any main functions for testing goes here

main::IO()
main = putStrLn (show (isPar 2678))