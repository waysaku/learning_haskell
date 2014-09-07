import Control.Monad
import Data.Char
import System.IO
import Control.Exception

{-
main = forever $ do
  l <- getLine
  putStrLn $ map toUpper l
-}

{-
main = do
  contents <- getContents
  putStr $ map toUpper contents
-}

{-
main = do
  contents <- getContents
  putStr (shortLinesOnly contents)
-}

{-
main = interact shortLinesOnly
shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
-}

{-
respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs then "palindrome" else "not a palindrome") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

main = interact respondPalindromes
-}

{-
main = do
  handle <- openFile "baabaa.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
-}

{-
main = do
  withFile "baabaa.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode) (\handle -> hClose handle) (\handle -> f handle)
-}

main = do
  contents <- readFile "baabaa.txt"
  writeFile "baabaa_Upper.txt" $ map toUpper contents







