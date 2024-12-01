import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.Monad
import Data.List (sort, foldl')
import qualified Data.Map.Strict as M
import Control.DeepSeq (deepseq)
import Control.Concurrent.Async (concurrently)

absAndSubtractTuple :: Num a => (a, a) -> a
absAndSubtractTuple = abs . uncurry (-)

toListOfTuples :: ([a], [b]) -> [(a, b)]
toListOfTuples = uncurry zip

sortEach :: ([Int], [Int]) -> ([Int], [Int])
sortEach (l1, l2) = (sort l1, sort l2)

toTupleOfLists :: String -> ([Int], [Int])
toTupleOfLists = unzip . sublistsToTuples . map (map read) . map words . lines
    where
        sublistsToTuples :: [[Int]] -> [(Int, Int)]
        sublistsToTuples = map (\[x, y] -> (x, y)) 

solvePart1 :: ([Int], [Int]) -> Int
solvePart1 = sum . map absAndSubtractTuple . toListOfTuples . sortEach

insertIntoFreqMap :: [Int] -> M.Map Int Int -> Int -> M.Map Int Int
insertIntoFreqMap vs m k =
    let newMap = case M.lookup k m of
            Nothing ->  -- number not found, insert its count
                let occurrences = length $ filter (== k) vs
                in M.insert k occurrences m
            Just _ -> m  -- number has already been inserted
    in newMap

solvePart2 :: ([Int], [Int]) -> Int
solvePart2 (l1, l2) = 
    let numbers = foldl' (insertIntoFreqMap l2) M.empty l1
    in foldl' (\acc (x, y) -> acc + (x * y)) 0 (M.toList numbers)

readAndParseFile :: FilePath -> IO ([Int], [Int])
readAndParseFile filePath = 
    withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        contents `deepseq` return (toTupleOfLists contents)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            tuples <- readAndParseFile filePath
            let part1Result = solvePart1 tuples
            let part2Result = solvePart2 tuples
            putStrLn $ "Part 1: " ++ show part1Result
            putStrLn $ "Part 2: " ++ show part2Result
        _ -> putStrLn "Usage: program <file-path>"
