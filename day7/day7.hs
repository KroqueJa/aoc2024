import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.DeepSeq (deepseq)
import Data.List.Split (splitOn)
import Control.Concurrent.Async
import Data.Maybe (catMaybes)

type Equation = (Int, [Int])

solve :: (Equation -> Bool) -> [Equation] -> IO Int
solve checkEquation = fmap (sum . catMaybes) . mapConcurrently (getIff checkEquation)
  where
    getIff :: (Equation -> Bool) -> Equation -> IO (Maybe Int)
    getIff check eq@(lhs, _) =
        return $ if check eq then Just lhs else Nothing

catNums :: Int -> Int -> Int
catNums a b = a * (10 ^ numDigits b) + b
  where
    numDigits :: Int -> Int
    numDigits 0 = 1  -- Special case for 0
    numDigits n = floor (logBase 10 (fromIntegral n)) + 1

-- Higher order function that takes other functions as inputs to check the equations
checkEquation :: [Int -> Int -> Int] -> Equation -> Bool
checkEquation ops (lhs, []) = lhs == 0
checkEquation ops (lhs, [rhs]) = lhs == rhs
checkEquation ops (lhs, (r1:r2:rs)) =
    any (\op -> checkEquation ops (lhs, op r1 r2 : rs)) ops

solvePart1 :: [Equation] -> IO Int
solvePart1 = solve (checkEquation [(+), (*)])

solvePart2 :: [Equation] -> IO Int
solvePart2 = solve (checkEquation [(+), (*), catNums])

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            equations <- readAndParseFile filePath

            solution1 <- solvePart1 equations
            print $ solution1

            solution2 <- solvePart2 equations
            print $ solution2
        _ -> print $ "Usage: day2 <input>"

-- | Reads and parses the input file into
readAndParseFile :: FilePath -> IO [Equation]
readAndParseFile filePath = 
    withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        contents `deepseq` return (parse contents)
    where
        parse :: String -> [Equation]
        parse = map parseEquation . lines

        parseEquation :: String -> Equation
        parseEquation s = let parts = splitOn ":" s in ((read $ head parts), (map read $ words $ last parts))
