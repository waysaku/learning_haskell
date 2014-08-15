doubleMe x = x + x

bmiTell :: Double -> Double -> String
bmiTell weight height
  |bmi <= skinny = "underWeight!"
  |bmi <= normal = "Normal"
  |bmi <= fat = "Fat!"
  |otherwise    = "Too Fat"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)



greet :: String -> String
greet name
  |name == "masami"    = niceGreeting ++ "Masami!"
  |name == "akari"     = niceGreeting ++ "Akari!"
  |name == "sakutaro"  = niceGreeting ++ "Sakutaro!"
  |otherwise           = badGreeting ++ " " ++ name
  where niceGreeting = "Hello! So very nice to see you, "
        badGreeting  = "Oh! Pfit. It's you."

initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l]
    where 
        (f:_) = firstname
        (l:_) = lastname

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 2.5]


describeList :: [a] -> String
describeList ls = "The list is "
                ++ case ls of [] -> "empty"
                              [x]-> "a singleton list"
                              xs -> "a longer list"


