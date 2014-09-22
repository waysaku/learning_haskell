import Data.Char
import Data.List
import Control.Applicative

{-
main = do 
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
-}

{-
main = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
-}

{-
main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line
-}

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)


main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines concatenated turn out to be: " ++ a





















