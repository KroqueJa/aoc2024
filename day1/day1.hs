import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.Monad
import Data.List (sort, foldl')
import qualified Data.Map.Strict as M
import Control.DeepSeq (deepseq)


-- | Solves part 1 of the problem by summing the absolute differences of corresponding pairs
-- in two sorted lists.
solvePart1 :: ([Int], [Int]) -> Int
solvePart1 = sum . map absAndSubtractTuple . toListOfTuples . sortEach
    where
        -- | Sorts each list in a pair of lists.
        sortEach :: ([Int], [Int]) -> ([Int], [Int])
        sortEach (l1, l2) = (sort l1, sort l2)

        -- | Converts a pair of lists into a list of tuples.
        toListOfTuples :: ([a], [b]) -> [(a, b)]
        toListOfTuples = uncurry zip

        -- | Computes the absolute difference of a tuple of two numbers.
        absAndSubtractTuple :: Num a => (a, a) -> a
        absAndSubtractTuple = abs . uncurry (-)


-- | Solves part 2 by pairing each element from the left list with its count in the right list in a map
solvePart2 :: ([Int], [Int]) -> Int
solvePart2 (l1, l2) = 
    let numbers = foldl' (insertIntoFreqMap l2) M.empty l1
    in foldl' (\acc (x, y) -> acc + (x * y)) 0 (M.toList numbers)
    where
        -- | Inserts a key into a frequency map, counting occurrences of that key in the input list.
        insertIntoFreqMap :: [Int] -> M.Map Int Int -> Int -> M.Map Int Int
        insertIntoFreqMap vs m k =
            let newMap = case M.lookup k m of
                    Nothing ->  -- number not found, insert its count
                        let occurrences = length $ filter (== k) vs
                        in M.insert k occurrences m
                    Just _ -> m  -- number has already been inserted
            in newMap

-- | Reads and parses the input file into a tuple of two lists.
readAndParseFile :: FilePath -> IO ([Int], [Int])
readAndParseFile filePath = 
    withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        -- `deepseq` forces an evaluation of all contents so that the file handle does not
        -- close before the contents have been read into memory.
        contents `deepseq` return (toTupleOfLists contents)
    where
        -- | Parses a string representation of pairs of integers into a tuple of two lists.
        toTupleOfLists :: String -> ([Int], [Int])
        toTupleOfLists = unzip . sublistsToTuples . map (map read) . map words . lines

        -- | Converts a list of sublists into a list of tuples, assuming each sublist has two elements.
        sublistsToTuples :: [[Int]] -> [(Int, Int)]
        sublistsToTuples = map (\[x, y] -> (x, y)) 

-- | The main function. Reads a file path from the command line arguments,
-- parses the file, and solves both parts of the problem.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            -- Parse input
            tuples <- readAndParseFile filePath
            -- Solve part 1
            let part1Result = solvePart1 tuples
            -- Solve part 2
            let part2Result = solvePart2 tuples
            -- Print results
            putStrLn $ "Part 1: " ++ show part1Result
            putStrLn $ "Part 2: " ++ show part2Result
        _ -> putStrLn "Usage: program <file-path>"
