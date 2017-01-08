module Second where

import Prelude hiding (Left, Right)
import qualified Data.Map.Strict as M

data Direction = Up | Down | Left | Right
    deriving (Show, Ord, Eq)

type Line = [Direction]

readInstructions :: String -> IO [Line]
readInstructions filename = do
    input <- readFile filename
    return $ map parseLine (lines input)

parseLine :: String -> Line
parseLine = map parseDirection

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Left
parseDirection 'R' = Right
parseDirection _ = undefined

move :: Int -> Direction -> Int
move i Left  | i `mod` 3 /= 1 = i - 1
move i Right | i `mod` 3 /= 0 = i + 1
move i Up    | i > 3          = i - 3
move i Down  | i < 7          = i + 3
move i _                      = i

getDigits :: Ord a => (a -> Direction -> a) -> a -> [Line] -> [a]
getDigits fm = scanl (foldl fm)

main :: IO ()
main = do
    ls <- readInstructions "input.txt"
    print $ tail $ getDigits move 5 ls
    print $ tail $ getDigits move' '5' ls

move' :: Char -> Direction -> Char
move' c d = M.findWithDefault c c (moveset M.! d)

moveset :: M.Map Direction (M.Map Char Char)
moveset = M.fromList [(Up,    M.fromList (zip u d)),
                      (Down,  M.fromList (zip d u)),
                      (Left,  M.fromList (zip l r)),
                      (Right, M.fromList (zip r l))]
                          where u = "3678ABCD"
                                d = "1234678B"
                                l = "637B48C9"
                                r = "526A37B8"
