module Fourth where

import Text.Regex.Posix
import qualified Data.Map.Strict as M
import Data.List (sortBy, find)
import Data.Char (ord, chr)

rxp :: Regex
rxp = makeRegex "^([^0-9]+)-([0-9]+)\\[([^]]+)\\]$"

newtype RoomName = RoomName String deriving Show
newtype SectorId = SectorId Int    deriving Show
newtype Checksum = Checksum String deriving (Show, Eq)

data Room = Room { name     :: RoomName
                 , sector   :: SectorId
                 , checksum :: Checksum
                 } deriving (Show)

readRooms :: String -> IO [Room]
readRooms filename = do
    input <- readFile filename
    return $ map parseRoom (lines input)

parseRoom :: String -> Room
parseRoom s = let [[_, n, i, c]] = match rxp s
               in Room (RoomName n) (SectorId (read i)) (Checksum c)

count :: M.Map Char Int -> Char -> M.Map Char Int
count m c = M.insertWith (+) c 1 m

calculateChecksum :: RoomName -> Checksum
calculateChecksum (RoomName rn) =
    Checksum $
      map fst
    . take checksumSize
    . sortBy cmp
    . M.assocs
    $ foldl count M.empty
    $ filter (/= '-') rn
        where cmp t1 t2    = case compare (snd t2) (snd t1) of
                               EQ -> compare (fst t1) (fst t2)
                               x  -> x
              checksumSize = 5

isValid :: Room -> Bool
isValid r = calculateChecksum (name r) == checksum r

main :: IO ()
main = do
    rs <- readRooms "input.txt"
    let npr = findNorthPoleRoom rs
        sid (SectorId s) = s
    print $ sum $ map (sid . sector) $ filter isValid rs
    print $ decryptedName <$> npr
    print npr

decryptedName :: Room -> RoomName
decryptedName r = RoomName (map cipher rn)
    where (RoomName rn) = name r
          (SectorId i) = sector r
          cipher c = case c of
                       '-' -> ' '
                       x   -> chr $ 97 + mod (ord x - 97 + i) 26

findNorthPoleRoom :: [Room] -> Maybe Room
findNorthPoleRoom = find q
    where q :: Room -> Bool
          q r = let (RoomName s) = decryptedName r
                 in s =~ "pole"
