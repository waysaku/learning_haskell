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
data Person = Person {firstName   :: String,
                      lastName    :: String,
                      age         :: Int,
                      height      :: Float,
                      phoneNumber :: String,
                      flavor      :: String} deriving (Show)


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

