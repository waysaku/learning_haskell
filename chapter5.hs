multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys



flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

largestDivisible :: Integer
largestDivisible = head(filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain(n `div` 2)
  | odd n = n : chain(n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter(\xs -> length xs > 15) (map chain [1..100]))
