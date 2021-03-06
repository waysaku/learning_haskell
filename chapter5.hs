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


sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr(\x acc -> f x : acc) [] xs


elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr(\x acc -> if x == y then True else acc) False ys


reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- filter' (\x -> x `mod` 2 == 0) [1,2,3,4,5,6]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr(\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- funct' :: (Int a) => a -> a
-- funct' x = x + 1

func' :: (Num a) => a -> [a]
func' x = replicate 5 x

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs

-- fn x = ceiling(negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
-- oddSquareSum = sum (takeWhile (<10000)(filter odd (map (^2)[1..])))
oddSquareSum = sum (takeWhile (<10000)(filter odd (map (^2) [1..] )))






