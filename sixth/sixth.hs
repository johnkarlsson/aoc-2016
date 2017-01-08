module Sixth where

import qualified Data.Map.Strict as M
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

type Counter = M.Map Char Int

readInput :: String -> IO [String]
readInput filename = do
    input <- readFile filename
    return (lines input)

count :: Counter -> Char -> Counter
count m c = M.insertWith (+) c 1 m

process :: [String] -> [Counter]
process = foldl (zipWith count) (replicate 8 M.empty)

main :: IO ()
main = do
    input <- process <$> readInput "input.txt"
    putStrLn $ char maximumBy <$> input
    putStrLn $ char minimumBy <$> input
        where
            char f = fst . f (comparing snd) . M.assocs
