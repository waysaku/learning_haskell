import Data.List
import Data.Char
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

searcharr :: (Ord a) =>  [a] -> [a] -> Bool
searcharr xs ys = length [x | x <- tails ys, x == xs] > 0

encode' :: Int -> String -> String
encode' n xs = map (\z -> chr z) $ map (\y -> y - n) $ map (\x -> ord x) xs
