maximum' :: (Ord a) => [a] -> a
maximum' []     = error"maximum of empty list!"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)


take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' n [] = []
take' n (x:xs) = x : take' (n - 1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat x


zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) 
  | n == x = True
  | otherwise = n `elem'` xs


-- [5,1,9,4,6,7,3]
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) = 
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in sort' smallerOrEqual ++ [x] ++ sort' larger
