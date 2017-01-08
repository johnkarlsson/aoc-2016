{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- module Ninth where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Monad (liftM)
import Data.List.Extra (trim)
import Control.Applicative ((<|>))

main :: IO ()
main = do
    let fn = "input.txt"
    ans <- read . trim <$> readFile fn :: IO UncompressedSize
    print ans

data UncompressedSize = UncompressedSize
    { version1 :: Int
    , version2 :: Int
    } deriving Show

instance Read UncompressedSize where
    readsPrec _ = readP_to_S parseLength

parseLength :: ReadP UncompressedSize
parseLength = do
    l <- many1 $ compressedText <|> regularText
    return $ UncompressedSize (sum $ map version1 l) (sum $ map version2 l)
        where
            compressedText = do
                let num = read <$> many1 (satisfy isDigit)
                (nChars, nTimes) <- between (char '(') (char ')') $ (,) <$> (num <* char 'x') <*> num
                s <- count nChars get
                let v1 = nTimes * length s
                    v2 = nTimes * version2 (read s)
                return (UncompressedSize v1 v2)
            regularText = do
                l <- liftM length $ munch1 (/= '(')
                return (UncompressedSize l l)
