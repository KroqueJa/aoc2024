import System.Environment (getArgs)
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.DeepSeq (deepseq)

import Data.List.Split (splitOn)
import Data.List (foldl')

import qualified Data.Set as S
import qualified Data.Map.Strict as M


-- An `Update` is a list of integers
type Update = [Int]

-- A `Rule` is a pair of integers, detailing that one must appear before the other
type Rule = (Int, Int)

-- A `Rulebook` specifies for each subjected number, a set of numbers it can't have already seen.
type Rulebook = M.Map Int (S.Set Int)

readAndParseFile :: FilePath -> IO ([Rule], [Update])
readAndParseFile filePath = 
    withFile filePath ReadMode $ \handle -> do
        contents <- hGetContents handle
        contents `deepseq` return (parse contents)
    where
        parse :: String -> ([(Int, Int)], [[Int]])
        parse s = 
            let
                (ruleStrings, pageStrings) = span (/="") $ lines s
                rules = map parseRuleString $ ruleStrings
                pages = map parsePageString $ filter (/="") $ pageStrings
            in (rules, pages)
        
        parseRuleString :: String -> (Int, Int)
        parseRuleString s = let sublist = map read $ splitOn "|" s in (head sublist, last sublist)

        parsePageString :: String -> [Int]
        parsePageString = map read . splitOn ","


-- | Compiles a list of rules into a rulebook 
compileRulebook :: [Rule] -> Rulebook
compileRulebook = insertRules M.empty
  where
    insertRules :: Rulebook -> [Rule] -> Rulebook
    insertRules = foldl' updateRulebook

    updateRulebook :: Rulebook -> Rule -> Rulebook
    updateRulebook book (k, v) =
      let existingSet = M.findWithDefault S.empty k book
          updatedSet = S.insert v existingSet
      in M.insert k updatedSet book

-- | Checks if an update is OK
checkUpdate :: Rulebook -> Update -> Bool
checkUpdate = checkUpdateRecursive S.empty
    where
        checkUpdateRecursive :: S.Set Int -> Rulebook -> Update -> Bool
        checkUpdateRecursive _ _ [] = True
        checkUpdateRecursive seen _rules (i:is) = case M.lookup i _rules of
            -- There exists no rule for this number, record that we've seen it
            Nothing -> let updatedSeen = S.insert i seen in checkUpdateRecursive updatedSeen _rules is
            -- There exists a rule - check if any seen numbers are in the set of forbidden numbers for this number
            Just forbidden ->
                let seenAndForbidden = S.intersection seen forbidden
                in if seenAndForbidden /= S.empty then False else checkUpdateRecursive (S.insert i seen) _rules is

-- | Extracts the middle element of a list (assuming blindly that it has an odd length)
middle :: [a] -> a
middle u = let trim = ((length u) - 1) `div` 2 in head $ drop trim u

-- | The solution for part 1 is to traverse each list (Update) together with a set of already seen numbers.
-- For each number, we check if a rule exists. If the rule exists, we check that the intersection of the seen numbers and the rule set is empty.
-- If the full Update is legal, we find the middle page and add it to the sum.
solvePart1 :: Rulebook -> [Update] -> Int
solvePart1 rules updates = sum $ map middle $ filter (checkUpdate rules) updates


-- | For part two, we extract the incorrect updates and attempt to correct them using the rules.
solvePart2 :: Rulebook -> [Update] -> Int
solvePart2 rules updates = sum $ map middle $ map (ruleBasedSort rules []) $ filter (\update -> not (checkUpdate rules update)) updates
    where
        -- | Orders numbers into "checked" and "not yet checked" lists. If a number is subject to a rule
        -- , it must be ordered back into the correct place in "checked".
        ruleBasedSort :: Rulebook -> Update -> Update -> Update
        ruleBasedSort _ checked [] = checked
        ruleBasedSort book checked (i:is) =
            case M.lookup i book of
                -- No rules for this number; append it to the checked list
                Nothing -> ruleBasedSort book (checked ++ [i]) is

                -- There exists a rule for this number
                Just forbidden ->
                    let
                        checkedSet = S.fromList checked
                        checkedHasForbidden = not (S.null (S.intersection checkedSet forbidden))
                    in if checkedHasForbidden
                        then ruleBasedSort book (insertBeforeAnyOf forbidden checked i) is
                        else ruleBasedSort book (checked ++ [i]) is

        -- | Inserts an element into a list before the first found element of the set
        insertBeforeAnyOf :: (Eq a, Ord a) => S.Set a -> [a] -> a -> [a]
        insertBeforeAnyOf constraintSet list e =
            let (before, after) = span (\a -> not (S.member a constraintSet)) list
            in before ++ [e] ++ after

main :: IO()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            (rules, pages) <- readAndParseFile filePath

            let rulebook = compileRulebook rules
            let solution1 = solvePart1 rulebook pages
            print solution1

            let solution2 = solvePart2 rulebook pages
            print solution2

        _ -> print $ "Usage: day2 <input>"
    
