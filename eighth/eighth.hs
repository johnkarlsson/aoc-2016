{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Eighth where

import Control.Monad (zipWithM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Array (elems, Array)
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isDigit)

readInput :: String -> IO [Instruction]
readInput filename = do
    input <- readFile filename
    return $ map read (lines input)

parseRotate :: ReadP Instruction
parseRotate = do
    string "rotate "
    typ <- string "row" <|> string "column"
    string " x=" <|> string " y="
    idx <- many1 (satisfy isDigit)
    string " by "
    rot <- many1 (satisfy isDigit)
    return $ case typ of 
               "row"    -> Rotate (Row (read idx)) (read rot)
               "column" -> Rotate (Column (read idx)) (read rot)
               _        -> undefined

parseRect :: ReadP Instruction
parseRect = do
    string "rect "
    w <- many1 (satisfy isDigit)
    string "x"
    h <- many1 (satisfy isDigit)
    return (Rect (read w) (read h))

instance Read Instruction where
    readsPrec _ = readP_to_S $ parseRotate <|> parseRect

maxHeight :: Int
maxHeight = 6 - 1

maxWidth :: Int
maxWidth = 50 - 1

type Arr s = STArray s (Int, Int) Bool

data Line = Column Int | Row Int
    deriving Show

data Instruction = Rotate Line Int | Rect Int Int
    deriving Show

getRange :: Line -> [(Int, Int)]
getRange (Column i) = range ((0, i), (maxHeight, i))
getRange (Row i)    = range ((i, 0), (i, maxWidth))

run :: Arr s -> Instruction -> ST s ()
run a (Rect x y) = zipWithM_ (writeArray a) (range ((0,0), (y - 1, x - 1))) (repeat True)
run a (Rotate l steps) = do
    let ids = getRange l
    vals <- mapM (readArray a) ids
    let bound = length vals
        newVals = take bound $ drop (bound - steps) $ cycle vals
    zipWithM_ (writeArray a) ids newVals

getBoard :: ST s (Arr s)
getBoard = newArray ((0,0), (maxHeight, maxWidth)) False :: ST s (Arr s)

main :: IO ()
main = do
    program <- readInput "input.txt"
    let output = runSTArray $ do
        arr <- getBoard
        mapM_ (run arr) program
        return arr
    printBoard output
    print $ length $ filter id (elems output)

printBoard :: Ix i => Array i Bool -> IO ()
printBoard a = mapM_ putStrLn $ batches 50 (map vis (elems a))
    where vis x = if x then '#' else ' '

batches :: Int -> [a] -> [[a]]
batches _ [] = []
batches i as = take i as : batches i (drop i as)
