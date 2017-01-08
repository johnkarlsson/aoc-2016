module Main where

import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn "hello world"
  print $ let ss = map (S.fromList . take 5000000) sols2 in foldl S.intersection (head ss) ss

sols = [ [15 + 17*x | x <- [0..]]
       , [5 + 7*y | y <- [0..]]
       , [14 + 19*z | z <- [0..]]
       , [1 + 5*w | w <- [0..]]
       , [1 + 3*h | h <- [0..]]
       , [2 + 13*i | i <- [0..]]]

sols2 = [4 + 11*j | j <- [0..]]:sols
