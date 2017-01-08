module Main where

import Data.List (sortBy)
import Data.Function (on)
import Data.Foldable (foldlM)

readLine s = read ("(" ++ map tr s ++ ")") where
    tr '-' = ','
    tr x   = x

main :: IO ()
main = do 
    input <- map readLine <$> lines <$> readFile "input.txt"
    let sol = solutionFrom input 0
    print sol
    print $ length $ part2 input $ sol

part2 _     (Right _) = []
part2 input (Left x)  = x : part2 input (solutionFrom input x)

type Input = [(Int, Int)]

solutionFrom :: Input -> Int -> Either Int Int
solutionFrom input i = foldlM f i sortedInput where
    sortedInput = sortBy (compare `on` fst) input
    f y (i, j)  = if y < i - 1
                     then Left  (y + 1)
                     else Right (max y j)
