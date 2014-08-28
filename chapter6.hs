import Data.List
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub



wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
