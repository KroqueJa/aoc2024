-- Seems like at least part 1 should be a fancy oneliner in haskell

import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.Monad
import Data.List (sort, foldl')
import qualified Data.Map.Strict as M

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
solvePart1 =  sum . map absAndSubtractTuple . toListOfTuples . sortEach


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

main = do
    withFile "input.txt" ReadMode $ \handle -> do
        contents <- hGetContents handle
        let tuples = toTupleOfLists contents
        print $ solvePart1 tuples

        print $ solvePart2 tuples
