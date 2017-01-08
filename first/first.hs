module First where

import Control.Monad.State
import qualified Data.Set as S

data Heading = North
             | East
             | South
             | West
             deriving (Show, Ord, Eq)

data Turn = LeftTurn | RightTurn
    deriving (Show)

data Position = Position Int Int Heading
    deriving (Show, Ord)

instance Eq Position where
    (Position x y _) == (Position x' y' _) = (x, y) == (x', y')

data Instruction = Instruction Turn Int
    deriving (Show)

instance Read Instruction where
    readsPrec _ ('L':i) = [(Instruction LeftTurn (read i), "")]
    readsPrec _ ('R':i) = [(Instruction RightTurn (read i), "")]
    readsPrec _ s = [(undefined, s)]

readInstructions :: String -> IO [Instruction]
readInstructions filename = do
    input <- readFile filename
    return $ map read . words $ filter (/= ',') input

travel :: Int -> Position -> Position
travel i (Position x y North) = Position x (y + i) North
travel i (Position x y South) = Position x (y - i) South
travel i (Position x y East)  = Position (x + i) y East
travel i (Position x y West)  = Position (x - i) y West

turn :: Heading -> Turn -> Heading
turn North LeftTurn  = West
turn North RightTurn = East
turn East  LeftTurn  = North
turn East  RightTurn = South
turn South LeftTurn  = East
turn South RightTurn = West
turn West  LeftTurn  = South
turn West  RightTurn = North

step :: Instruction -> Position -> Position
step (Instruction t i) (Position x y h) = travel i (Position x y h')
    where h' = turn h t

distance :: Position -> Int
distance (Position x y _) = abs x + abs y

main1 :: IO ()
main1 = do
    instructions <- readInstructions "input.txt"
    let finalPosition = execState (mapM_ (modify . step) instructions) (Position 0 0 North)
    print finalPosition
    print $ distance finalPosition

-----

main2 :: IO ()
main2 = do
    instructions <- readInstructions "input.txt"
    let visitedPositions = scanl (flip step) (Position 0 0 North) instructions
        allVisitedPositions = intermediatePositions visitedPositions
    let rp = revisitedPosition allVisitedPositions
    print rp
    print $ distance <$> rp

intermediatePositions :: [Position] -> [Position]
intermediatePositions [] = []
intermediatePositions [p] = [p]
intermediatePositions (Position x y _:stop@(Position _ _ h):ps) =
    takeWhile (/= stop) (iterate (travel 1) (Position x y h))
    ++ intermediatePositions (stop:ps)

-- The first position that is visited twice
revisitedPosition :: [Position] -> Maybe Position
revisitedPosition = f S.empty
    where
        coords (Position x y _) = (x, y)
        f _ [] = Nothing
        f s (p:ps)
          | S.member (coords p) s = Just p
          | otherwise             = f (S.insert (coords p) s) ps
