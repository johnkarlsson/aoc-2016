module Main where

import qualified Data.Set as S
import Control.Monad.State
import Control.Monad (zipWithM_, mapM_)
import Data.List (intercalate, findIndex)
import Data.Maybe (mapMaybe)
import qualified Data.PriorityQueue.FingerTree as Q


-- Base
-------

data Item = Elevator | Generator String | Microchip String
    deriving (Eq, Ord)
type Floor = S.Set Item
data Direction = Up | Down
    deriving (Show, Eq)
type House = [Floor]

instance Show Item where
    show Elevator = "E"
    show (Generator xs) = 'G':' ':take 2 xs
    show (Microchip xs) = 'M':' ':take 2 xs

initialState :: House
initialState = map S.fromList [
                 []
               , [Microchip "cobalt", Microchip "curium", Microchip "ruthenium", Microchip "plutonium"]
               , [Generator "cobalt", Generator "curium", Generator "ruthenium", Generator "plutonium"]
               , [Elevator, Generator "promethium", Microchip "promethium"]
               ]

extras :: S.Set Item
extras = S.fromList [Generator "elerium", Microchip "elerium",
                     Generator "dilithium", Microchip "dilithium"]

initialState2 :: House
initialState2 = h where
    (Right f) = getFloor initialState 3
    (Right h) = replaceFloor 3 f' initialState
    f'        = S.union f extras

exampleState :: House
exampleState = map S.fromList [ []
                              , [Generator "Lithium"]
                              , [Generator "Hydrogen"]
                              , [Elevator, Microchip "Hydrogen", Microchip "Lithium"]
                              ]

printHouse :: House -> IO ()
printHouse = zipWithM_ printFloor [0..] where
    printFloor x f = putStrLn $ show x ++ ":\t" ++ intercalate ",\t" (map show (S.toList f))


-- Game rules
-------------

type Error = String

heuristic :: House -> Int
heuristic h = sum $ zipWith g (S.size . withoutElevator <$> h) [0..] where
    g _ 0 = 0
    g 0 _ = 0
    g n f = let n' = 1 + (2 * (n - 2))
             in n * fac f
    fac x = if x < 2 then 1 else x * fac (x - 1)
-- heuristic h = sum $ zipWith (*) (map S.size h) [0..]

-- | If a floor has a generator, all microchips must be matched with a generator
legalState :: House -> Bool
legalState = all legalFloor where
    legalFloor f = not $ S.size gs > 0 && S.size (ms S.\\ gs) > 0 where
        gs = S.fromList [s | Generator s <- S.toList f]
        ms = S.fromList [s | Microchip s <- S.toList f]

moveItems :: Int -> Direction -> House -> S.Set Item -> Either Error House
moveItems i d h is
  | i < 0 || i >= l                   = Left "Must move existing floor"
  | i == 0 && d == Up                 = Left "May not move past bounds"
  | i == (l - 1) && d == Down         = Left "May not move past bounds"
  | not $ S.member Elevator is        = Left "Must move incl. elevator"
  | not $ is `S.isSubsetOf` (h !! i)  = Left "Must move items on floor"
  | otherwise                         = pure h >>= fU >>= f >>= fD >>= ensureLegal
    where l = length h
          iU = i - 1
          iD = i + 1
          f  =                             replaceFloor i  (h !! i  S.\\      is)
          fU = if d == Down then pure else replaceFloor iU (h !! iU `S.union` is)
          fD = if d == Up   then pure else replaceFloor iD (h !! iD `S.union` is)
          ensureLegal h = if legalState h
                             then Right h
                             else Left "Generator fries microchip"

replaceFloor :: Int -> Floor -> House -> Either Error House
replaceFloor i f h
  | i < 0 || i >= l = Left "replaceFloor out of bounds"
  | otherwise       = Right $ take i h ++ [f] ++ drop (i + 1) h
    where l = length h

getFloor :: House -> Int -> Either Error Floor
getFloor h i
  | i < 0 || i >= l = Left "getFloor out of bounds"
  | otherwise       = Right (h !! i)
    where l = length h

-- | Move the elevator one floor up and down, and bring one or two items
neighbours :: House -> [House]
neighbours h = maybe [] neighbours' (findIndex (S.member Elevator) h) where
    neighbours' i = mapMaybe rightToMaybe eitherHouses where
        eitherHouses = concatMap f [Up, Down] where
            f dir = map (moveItems i dir h . withElevator) (movableItemSets (h !! i))

withElevator :: Floor -> Floor
withElevator = S.insert Elevator

withoutElevator :: Floor -> Floor
withoutElevator = S.delete Elevator

movableItemSets :: Floor -> [S.Set Item]
movableItemSets f = map S.fromList $ subsets (S.toList (withoutElevator f)) 2

-- | At least 1 element. Elevator may not go empty.
subsets :: [a] -> Int -> [[a]]
subsets xs i = concatMap (choose xs) [1..i] where
    _      `choose` 0       = [[]]
    []     `choose` _       =  []
    (x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

rightToMaybe :: Either Error House -> Maybe House
rightToMaybe (Right x) = Just x
rightToMaybe (Left _)  = Nothing


-- A*
-----

type PathLength = Int
type Node = (House, PathLength)
type Priority = Int
type Explored = S.Set House
type Frontier = Q.PQueue Priority Node

instance Ord k => Show (Q.PQueue k v) where
    show q
      | null q    = "<Empty Queue>"
      | otherwise = "<Queue>"

isGoal :: House -> Bool
isGoal = all S.null . tail

search :: Explored -> StateT Frontier Maybe Node
search seen = do
    n@(h, _) <- pop seen
    if isGoal h
       then return n
       else do
           expand n
           search (S.insert h seen)

-- | Pop unseen
pop :: Explored -> StateT Frontier Maybe Node
pop seen = do
    n@(h, _) <- StateT Q.minView
    if h `S.member` seen then pop seen else return n

push :: Node -> StateT Frontier Maybe ()
push n@(h, p) = StateT $ Just . (,) () . Q.insert (p + heuristic h) n

expand :: Node -> StateT Frontier Maybe ()
expand n@(h, p) = mapM_ push [(h', p + 1) | h' <- neighbours h]


-- Main
-------

searchFrom :: House -> Maybe Node
searchFrom h = evalStateT (search S.empty) (Q.singleton 0 (h, 0))

main :: IO ()
main = print $ searchFrom initialState
