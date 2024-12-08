import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.DeepSeq (deepseq)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

type Position = (Int, Int)

data Grid = Grid { height :: Int, width :: Int, antennas :: M.Map Char [Position] } deriving (Show)

-- General solver
solve :: (Position -> Position -> S.Set Position) -> Grid -> Int
solve pairToAntinodes = S.size . antinodes
  where
    -- | Calculates all antinodes in a grid
    antinodes :: Grid -> S.Set Position
    antinodes grid@(Grid h w a) = S.filter (inBounds h w) $ S.unions $ map allAntinodes $ M.elems a

    -- | Checks if a Position is in bounds of a height and width
    inBounds :: Int -> Int -> Position -> Bool
    inBounds h w (r, c) = r >= 0 && c >= 0 && r < h && c < w

    -- | Checks all antinodes in a list, and puts them in a set
    allAntinodes :: [Position] -> S.Set Position
    allAntinodes ps = S.unions $ map (\p -> S.unions [pairToAntinodes p q | q <- ps, q /= p]) ps

-- Solvers
solvePart1 :: Grid -> Int
solvePart1 = solve antennaPairToAntinodesPart1
    where
        -- | Creates antinodes for an antenna pair according to part 1
        antennaPairToAntinodesPart1 :: Position -> Position -> S.Set Position
        antennaPairToAntinodesPart1 p1@(r1, c1) p2@(r2, c2) =
            let
                dr = r2 - r1
                dc = c2 - c1

                -- "Two steps from p1"
                antinode1 = (r1 + 2*dr, c1 + 2*dc)

                -- "One step back from p1"
                antinode2 = (r1 - dr, c1 - dc)
            in
                S.fromList [antinode1, antinode2]

solvePart2 :: Grid -> Int
solvePart2 grid@(Grid h w as) = solve (antennaPairToAntinodesPart2 h w) grid
    where
        -- | Creates antinodes for an antenna pair according to part 2
        -- Just takes a large number of steps forward and backward, knowing that they will be filtered later
        antennaPairToAntinodesPart2 :: Int -> Int -> Position -> Position -> S.Set Position
        antennaPairToAntinodesPart2 h w p1@(r1, c1) p2@(r2, c2) =
            let
                largestDim = max h w
                dr = r2 - r1
                dc = c2 - c1

                -- "Fifty in the forward direction from p1"
                forwardAntinodes = [(r1 + k * dr, c1 + k * dc) | k <- [1..largestDim]]

                -- "Fifty in the backward direction from p2"
                backwardAntinodes = [(r2 - k * dr, c2 - k * dc) | k <- [1..largestDim]]
            in
                S.fromList $ forwardAntinodes ++ backwardAntinodes

-- | Main function and parser
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            grid <- readAndParseFile filePath
            
            print $ solvePart1 grid
            print $ solvePart2 grid
        _ -> print $ "Usage: day8 <input>"
    where
        -- | Reads and parses the input file into a list of Equation
        readAndParseFile :: FilePath -> IO Grid
        readAndParseFile filePath = 
            withFile filePath ReadMode $ \handle -> do
                contents <- hGetContents handle
                contents `deepseq` return (parse contents)
            where
                parse :: String -> Grid
                parse = addLinesToGrid (Grid 0 0 M.empty) . lines

                addLinesToGrid :: Grid -> [String] -> Grid
                addLinesToGrid grid lines = foldl' addLineToGrid grid (zip [0..] lines)

                addLineToGrid :: Grid -> (Int, String) -> Grid
                addLineToGrid grid@(Grid height width m) (lineNbr, line) =
                    let
                        antennas = parseAntennasFromLine lineNbr 0 line -- [(Char, Position)]
                        antennasMap = foldl' insertPair M.empty antennas -- M.Map Char [Position]
                        newMap = M.unionWith (++) m antennasMap     -- M.Map Char [Position]
                    in
                        Grid (height+1) (length line) newMap

                inBounds :: Int -> Int -> Position -> Bool
                inBounds height width (r, c) = r >= 0 && c >= 0 && r < height && c < width


                insertPair :: (Ord a) => M.Map a [b] -> (a, b) -> M.Map a [b]
                insertPair m (k, v) = M.insertWith (++) k [v] m

                parseAntennasFromLine :: Int -> Int -> String -> [(Char, Position)]
                parseAntennasFromLine _ _ "" = []
                parseAntennasFromLine row col (c:cs)
                    | c /= '.' = (c, (row, col)) : parseAntennasFromLine row (col+1) cs
                    | otherwise = parseAntennasFromLine row (col+1) cs
