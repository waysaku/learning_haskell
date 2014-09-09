solveRPN :: String -> Double
solveRPN = head . foldl foldingFunc [] . words
  where foldingFunc (x:y:ys) "*" = (y * x):ys
        foldingFunc (x:y:ys) "+" = (y + x):ys
        foldingFunc (x:y:ys) "-" = (y - x):ys
        foldingFunc xs numberString = read numberString:xs
  

data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, 
                    Section 5 90 20,
                    Section 40 2 25,
                    Section 10 8 0
                    ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]



























