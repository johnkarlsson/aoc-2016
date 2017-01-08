module Main where

import Data.List.Split (chunksOf)

getInput i s = take i $ until (\x -> length x >= i) f s where
    f xs = xs ++ "0" ++ let f '1' = '0'
                            f '0' = '1'
                         in f <$> (reverse xs)

checksum :: String -> String
checksum s = map f (chunksOf 2 s) where
    f x = if g x then '1' else '0'
    g   = all <$> (==) . head <*> tail

checksumN :: Int -> String -> String
checksumN steps s = concatMap f (chunksOf (2^steps) s) where
    f x = last $ take steps $ iterate checksum x


input1 = getInput 272 "10111011111001111"
input2 = getInput 35651584 "10111011111001111"

solution1 = checksumN 5 input1
solution2 = checksumN 22 input2

main = do
    print solution1
    print solution2
