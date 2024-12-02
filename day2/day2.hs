import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.DeepSeq (deepseq)

-- | Reads and parses the input file into a tuple of two lists.
readAndParseFile :: FilePath -> IO [[Int]]
readAndParseFile filePath = 
    withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        contents `deepseq` return (parse contents)
    where
        parse :: String -> [[Int]]
        parse = map (map read) . map words . lines


-- | Finds whether the difference between two elements is between [lb, ub].
-- The two elements are expressed as a tuple in order to not need to `uncurry` the call above.
diffBetween :: (Num a, Ord a) => a -> a -> (a, a) -> Bool
diffBetween lb ub (x, y) = let diff = y - x in diff >= lb && diff <= ub

-- | Reverse a list if it is overall decreasing.
reverseIfDecreasing :: (Ord a) => [a] -> [a]
reverseIfDecreasing xs = if head xs > last xs then reverse xs else xs

isSafe :: [Int] -> Bool
isSafe xs = all (diffBetween 1 3) (zip xs (tail xs))


-- | We reverse each sublist if it is decreasing overall, then check them against the criteria.
-- We are looking for the amount of "safe" lists, expressed as the length of a list of booleans.
solvePart1 :: [[Int]] -> Int
solvePart1 = length . filter isSafe . map reverseIfDecreasing

-- | For part two, we do the same as for part one but we generate all sublists that drop a single value, and check on those.
solvePart2 :: [[Int]] -> Int
solvePart2 = length . filter isAlmostSafe . map reverseIfDecreasing
  where
    isAlmostSafe :: [Int] -> Bool
    isAlmostSafe xs = any isSafe (allRemoved xs)

    allRemoved :: [a] -> [[a]]
    allRemoved xs = [deleteAt i xs | i <- [0 .. length xs - 1]]

    deleteAt :: Int -> [a] -> [a]
    deleteAt i xs = take i xs ++ drop (i + 1) xs

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

            let solution1 = solvePart1 tuples
            let solution2 = solvePart2 tuples

            print $ "Part 1: " ++ show solution1
            print $ "Part 2: " ++ show solution2

        _ -> print $ "Usage: day2 <input>"
