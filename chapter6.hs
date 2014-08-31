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

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

phoneBook = [
  ("betty", "555-2938"), 
  ("bonnie", "452-2928"), 
  ("patsy", "493-2928"), 
  ("lucille", "205-2928"), 
  ("wendy", "939-8282"), 
  ("penny", "853-2492")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs
