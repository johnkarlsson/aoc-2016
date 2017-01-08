import Data.Hash.MD5
import Data.List.Utils
import Data.Char
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import System.IO

hashes :: [String]
hashes = [md5s (Str ("reyedfim" ++ show i)) | i <- [(1 :: Int)..]]

candidates :: [String]
candidates = filter ((&&) <$> isOctDigit . (!! 5) <*> startswith "00000") hashes

search :: [MVar String] -> IO ()
search ms = forM_ candidates $ \h ->
    tryPutMVar (ms !! digitToInt (h !! 5)) [h !! 6]

main :: IO ()
main = do
    digits <- replicateM 8 newEmptyMVar
    _ <- forkIO (search digits)
    hSetBuffering stdout NoBuffering
    putStr "The passcode is: "
    forM_  digits (putStr <=< takeMVar)
    putStrLn ""
