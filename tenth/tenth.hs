{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map.Strict as M
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TBQueue
import Control.Monad (forM_, forever, when)
import Control.Monad.STM
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (foldl', sort)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

type Microchip = Int

data UnitId = BotId Int | OutputId Int
    deriving (Show, Eq, Ord)

data Unit where
    Bot    :: { bId :: UnitId , lo :: UnitId , hi :: UnitId } -> Unit
    Output :: { oId :: UnitId }                               -> Unit
    deriving (Show)

data Instruction = Init Microchip UnitId | NewUnit Unit
    deriving Show

instance Read Instruction where
    readsPrec _ = readP_to_S $ readInit <|> readBot where
        readInit = Init <$> (string "value " *> num)
                        <*> (BotId <$> (string " goes to bot " *> num))
        readBot  = NewUnit <$> (Bot <$> (BotId <$> (string "bot " *> num))
                                    <*> (string " gives low to "  *> receiver)
                                    <*> (string " and high to "   *> receiver))
        receiver = botId <|> outputId
        botId    = BotId    <$> (string "bot "    *> num)
        outputId = OutputId <$> (string "output " *> num)
        num      = read <$> many1 (satisfy isDigit)

type UnitMap = M.Map UnitId Unit

insertUnit :: UnitMap -> Unit -> UnitMap
insertUnit m u = M.insert (uId u) u m
    where uId Bot{bId=x}    = x
          uId Output{oId=x} = x

unitmap :: [Instruction] -> UnitMap
unitmap is = foldl' insertUnit M.empty us where
    us = concatMap units [b | NewUnit b@(Bot{}) <- is]
    units b@(Bot {hi=h, lo=l}) =
        catMaybes [Just b, mo h, mo l] where
            mo i@(OutputId _) = Just $ Output i
            mo _              = Nothing
    units _ = []

type Queue = TBQueue Microchip
type QueueMap = M.Map UnitId Queue

queuemap :: UnitMap -> STM QueueMap
queuemap = sequenceA . M.map (const $ newTBQueue 2)

give :: Microchip -> UnitId -> QueueMap -> STM ()
give c i m = mapM_ ((flip writeTBQueue) c) $ M.lookup i m

runInit :: QueueMap -> Instruction -> STM ()
runInit m (Init c i) = give c i m
runInit _ _          = return ()

worker :: QueueMap -> Unit -> IO ()
worker m (b@(Bot {})) = forever $ atomically $ do
    let mq = M.lookup (bId b) m
    forM_ mq $ \q -> do
        x  <- readTBQueue q
        my <- tryReadTBQueue q
        case my of
          Nothing -> retry -- No second value yet
          Just y  -> do
            let [mn, mx] = sort [x, y]
            when (mx == 61 && mn == 17) $ trace ("Answer: " ++ show b) pure ()
            give mx (hi b) m
            give mn (lo b) m
worker _ _            = return ()

findPartTwo :: QueueMap -> IO ()
findPartTwo m = print =<< computation where
    computation = atomically $ sequenceA $ do
        qs <- sequence [M.lookup (OutputId i) m | i <- [0..2]]
        return $ product <$> mapM readTBQueue qs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let is = map read (lines input)
        um = unitmap is
    qm <- atomically $ queuemap um
    atomically $ mapM_ (runInit qm) is
    mapM_ (forkIO . worker qm) (M.elems um)
    findPartTwo qm
