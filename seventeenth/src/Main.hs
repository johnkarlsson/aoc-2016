module Main where

import Prelude hiding (Left, Right)

import qualified Data.Hash.MD5 as MD5

import qualified Data.Set as S
import Control.Monad.State
import Data.List (find)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad (mapM_, zipWithM)
import qualified Data.PriorityQueue.FingerTree as Q

-- Game rules
-------------

puzzleInput = "udskfozm"
part1 = False  -- If False, will modify A* to brute force by ignoring seen and
               -- calculating cost differently

neighbours :: Node -> [Node]
neighbours ((p, ds), h) = mapMaybe (neighbour h p) ds

md5 :: String -> String
md5 = MD5.md5s . MD5.Str

getDirections :: History -> OpenDirections
getDirections history = catMaybes $ zipWith md m [U, D, L, R]
    where m      = take 4 $ md5 $ puzzleInput ++ concatMap show history
          md c d = find (== c) "bcdef" >> return d

neighbour :: History -> Position -> Direction -> Maybe Node
neighbour h p d = f p d
  where h'         = h ++ [d]
        node p'    = Just ((p', getDirections h'), h')
        f (x, y) U = if y > 0 then node (x, y - 1) else Nothing
        f (x, y) R = if x < 3 then node (x + 1, y) else Nothing
        f (x, y) D = if y < 3 then node (x, y + 1) else Nothing
        f (x, y) L = if x > 0 then node (x - 1, y) else Nothing

heuristic :: Node -> Int
heuristic (((x, y), _), _) = 3 - x + y - 3

cost :: Node -> Int
cost (ns, h) = sign * length h where
    sign = if (isGoal ns || part1) then 1 else (-1)


-- A*
-----

type Position = (Int, Int)
data Direction = U | R | D | L
    deriving (Ord, Eq, Show)
type OpenDirections = [Direction]
type NodeState = (Position, OpenDirections)
type History = [Direction]
type Node = (NodeState, History)
type Priority = Int
type Explored = S.Set NodeState
type Frontier = Q.PQueue Priority Node

instance Ord k => Show (Q.PQueue k v) where
    show q
      | null q    = "<Empty Queue>"
      | otherwise = "<Queue>"

isGoal :: NodeState -> Bool
isGoal (p, _) = p == (3,3)

search :: Explored -> StateT Frontier Maybe Node
search seen = do
    n@(h, _) <- pop (if part1 then seen else S.empty)
    empty <- gets Q.null
    if isGoal h && (part1 || empty)
       then return n
       else do
           unless (isGoal h) (expand n)
           search (S.insert h seen)

-- | Pop unseen
pop :: Explored -> StateT Frontier Maybe Node
pop seen = do
    n@(h, _) <- StateT Q.minView
    if h `S.member` seen then pop seen else return n

push :: Node -> StateT Frontier Maybe ()
push n = StateT $ Just . (,) () . Q.insert (cost n + heuristic n) n

expand :: Node -> StateT Frontier Maybe ()
expand n = mapM_ push (neighbours n)


-- Main
-------

getNode p = (ns, []) where ns = (p, getDirections [])

searchFrom :: Position -> Maybe Node
searchFrom p = evalStateT (search S.empty) (Q.singleton 0 n) where n = getNode p

main :: IO ()
main = do
    let Just ((_, _), ds) = searchFrom (0,0)
    print $ (if part1 then id else show . length) $ concatMap show ds
