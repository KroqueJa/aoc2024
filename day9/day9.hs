-- Easily my garbagest submission; super slow and messy and complex for part 2. I don't have the
-- motivation to fix it now, and probably ever... I want to move on. Sigh.

import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.DeepSeq (deepseq)

import Data.Char (digitToInt, isDigit, intToDigit)
import Data.Maybe (catMaybes, fromJust)
import Data.List (span, foldl', find)

import Data.List.Split (splitOn)
import Debug.Trace

type Block = (Int, [Maybe Int])


showBlock :: Block -> String
showBlock = getStr . snd
    where
        getStr :: [Maybe Int] -> String
        getStr [] = ""
        getStr (m:ms)
            | m == Nothing = '.' : getStr ms
            | otherwise = intToDigit (fromJust m) : getStr ms

showBlocks :: [Block] -> String
showBlocks = foldl' (++) "" . map showBlock

showIndexedBlocks :: [(Int, Block)] -> String
showIndexedBlocks = foldl' (++) "" . map showBlock . map snd


fileOrGap :: (Int, [Int]) -> [Maybe Int]
fileOrGap (n, lst) = if even n then map Just lst else map (\_ -> Nothing) lst

defrag :: Block -> [Maybe Int] -> [Maybe Int]
defrag (n, []) _ = []
defrag (n, (f:fs)) (i:is)
    | n == 0 = Nothing : defrag (0, fs) (i:is)
    | f == Nothing = i : defrag ((n-1), fs) is
    | otherwise = f : defrag ((n-1), fs) (i:is)

checksum :: [Int] -> Int
checksum lst = let withIndex = zip [0..] lst in foldr (\(x, y) acc -> x * y + acc) 0 withIndex

process :: [Block] -> [Block]
process blocks = process' blocks (reverse blocks)
  where
    process' :: [Block] -> [Block] -> [Block]
    process' processed [] = processed
    process' processed (b:bs) = 
        if Nothing `elem` (snd b) 
        then process' processed bs -- Skip blocks with `Nothing`
        else
            let
                -- Split the processed list into left and right based on the insertion point
                splitBlocks = splitOn [b] processed -- [[Block]]
                left = head splitBlocks             -- [Block] to the left of the insertion candidate
                right = last splitBlocks            -- [Block] to the right
                -- Insert the block into the left part
                updatedLeft = insertBlock b left
                candidate = updatedLeft ++ right
                candidateWithGap = updatedLeft ++ makeGap (fst b) ++ right
            in if candidate == processed 
               then process' processed bs
               else process' candidateWithGap bs

    makeGap :: Int -> [Block]
    makeGap i = [(i, take i (repeat Nothing))]

insertBlock :: Block -> [Block] -> [Block]
insertBlock block@(blockSize, files) blocks =
    let
        (tooSmall, rightHere) = span (\(sz, gap) -> Nothing `notElem` gap || sz < blockSize) blocks
    in
        if null rightHere 
        then blocks ++ [block] -- No suitable gap; append block to the end
        else 
            let 
                (fill, newGap) = merge block (head rightHere)
                -- Add the leftover gap only if the block was successfully inserted
                remaining = filter (\(x, _) -> x /= 0) (newGap : tail rightHere)
            in tooSmall ++ (fill : remaining)
    where
        -- Inserts the left list into the right, splitting out remaining `Nothing` into a new list
        merge :: Block -> Block -> (Block, Block)
        merge incoming@(incSz, incFiles) gap@(gapSz, gapPlaces) =
            let
                newGapPlaces = zipWith (\place file -> if place == Nothing then file else place) gapPlaces incFiles
                filled = take incSz newGapPlaces
                remainingGapPlaces = drop incSz gapPlaces
                newGapSz = length (filter (== Nothing) remainingGapPlaces)
                newGap = (newGapSz, remainingGapPlaces)
            in 
                if incSz <= gapSz -- Only merge if there is space for the incoming block
                then ((incSz, filled), newGap)
                else (gap, gap) -- Return the original gap unchanged if merge fails

checksumFromBlocks :: [Block] -> Int
checksumFromBlocks blocks =
    let
        concatenated = concat $ map snd blocks  -- [Maybe Int]
        withIdx = zip [0..] concatenated        -- [(Int, Maybe Int)]
    in
        foldl' (\acc (idx, val) -> acc + if val /= Nothing then idx * fromJust val else 0) 0 withIdx


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            numbers <- readAndParseFile filePath

            let files = map fileOrGap $ zip [0..] $ map (\(x, y) -> (replicate y) x) $ zip (concatMap (replicate 2) [0..]) numbers
            let n = length $ catMaybes $ concat files
            let filesystem = (n, concat files)

            -- Part 1
            let reserve = filter (/= Nothing) $ reverse $ concat files
            let defragged = catMaybes $ defrag filesystem reserve

            print $ checksum defragged

            -- Part 2
            let blocks = filter (\(sz, _) -> sz /= 0) $ map (\x -> (length x, x)) files

            -- For each number, from the end, try to place it exactly once
            let defragged2 = process blocks

            -- Calculate the checksum
            print $ checksumFromBlocks $ defragged2

        _ -> print $ "Usage: day9 <input>"

-- | Reads and parses the input file into a list of integers
readAndParseFile :: FilePath -> IO [Int]
readAndParseFile filePath = 
    withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        contents `deepseq` return (parse contents)
    where
        parse :: String -> [Int]
        parse = map digitToInt . filter isDigit
