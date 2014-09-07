import Control.Monad
import Data.Char
import System.IO
import Control.Exception
import System.Directory
import Data.List
import System.Environment

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

{-
main = do
  contents <- readFile "baabaa.txt"
  writeFile "baabaa_Upper.txt" $ map toUpper contents
-}


{-
main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
-}

{-
main = do
  contents <- readFile "todo.txt"
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ "-" ++ line) [0..] todoTasks
  putStrLn "These are your ToDo items: "
  mapM_ putStrLn numberedTasks
  putStrLn "Which one do you want to delete ? "
  nuberString <- getLine
  let number = read nuberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      removeFile "todo.txt"
      renameFile tempName "todo.txt")
-}


dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

main = do
  (command:argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, nuberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ "-" ++ line) [0..] todoTasks
  putStrLn "These are your ToDo items: "
  mapM_ putStrLn numberedTasks
  let number = read nuberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      removeFile "todo.txt"
      renameFile tempName "todo.txt")



















