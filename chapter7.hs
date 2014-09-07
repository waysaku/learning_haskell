import qualified Data.Map as M

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- area :: Shape -> Float
-- area (Circle _ _ r) = pi * r ^ 2
-- area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 -y1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle(Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle( Point(x1 + a)(y1 + b))(Point (x2 + a)(y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle(Point 0 0)(Point width height)




-- data Person = Person String String Int Float String String deriving (Show)
{-
data Person = Person {firstName   :: String,
                      lastName    :: String,
                      age         :: Int,
                      height      :: Float,
                      phoneNumber :: String,
                      flavor      :: String} deriving (Show)
-}

{-
data Car = Car { company :: String,
                  model :: String,
                  year :: Int} deriving (Show)

tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y}) = 
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
-}


data Car a b c = Car {company :: a,
                      model :: b,
                      year :: c} deriving(Show)

tellCar' :: (Show a) => Car String String a -> String
tellCar' (Car {company = c, model = m, year = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector ( i + l) (j + m) (k + n)

dotProd :: (Num a ) => Vector a -> Vector a -> a
(Vector i j k ) `dotProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)



data Person = Person {firstName :: String,
                      lastName :: String,
                      age :: Int
                      } deriving (Eq, Show, Read)

yusaku   = Person {firstName = "yusaku",   lastName = "watanabe", age = 36}
masami   = Person {firstName = "masami",   lastName = "shime",    age = 37}
akari    = Person {firstName = "akari",    lastName = "watanabe", age = 5}
sakutaro = Person {firstName = "sakutaro", lastName = "watanabe", age = 3}


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook = [
  ("betty", "555-2938"), 
  ("bonnie", "452-2928"), 
  ("patsy", "493-2928"), 
  ("lucille", "205-2928"), 
  ("wendy", "939-8282"), 
  ("penny", "853-2492")
  ]

-- type PhoneBook   = [(String, String)]
type PhoneNumber = String
type Name        = String
type PhoneBook   = [(Name, PhoneNumber)]


inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook


type AssocList k v = [(k, v)]

type IntMap v = M.Map Int v




data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = M.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case M.lookup lockerNumber map of
  Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) -> if state /= Taken
    then Right code
    else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = M.fromList [
  (100, (Taken, "ZD391")),
  (101, (Free,  "JAH3I")),
  (103, (Free,  "IQSA9")),
  (105, (Free,  "QOTSA")),
  (109, (Taken, "893JJ")),
  (110, (Taken, "99292"))
  ]




-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)




data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red    == Red    = True
  Green  == Green  = True
  Yellow == Yellow = True
  _      == _      = False

instance Show TrafficLight where 
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

{-
instance Eq (Maybe m) where
  Just x  == Just y  = x == y
  Nothing == Nothing = True
  _       == _       = False
-}

{-
instance (Eq m) => Eq (Maybe m) where
  Just x  == Just y  = x == y
  Nothing == Nothing = True
  _       == _       = False
-}


class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult





instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
























