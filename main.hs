import Html
import System.Console.Terminfo (Color (Magenta, Yellow))
import Data.Maybe (listToMaybe)

replicate_ :: Int -> a -> [a]
replicate_ numEls toReplicate =
  if numEls <= 0
    then []
    else toReplicate : replicate_ (numEls - 1) toReplicate

{-- Mutual Recursion (two or more fns call each other recursively):

--}

odd_ :: Int -> Bool
odd_ n =
  case n of
    0 -> False
    x -> even_ (x - 1)

even_ :: Int -> Bool
even_ n =
  case n of
    0 -> True
    x -> odd_ (x - 1)

data Brightness = Dark | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor = AnsiColor Brightness EightColor

isBright :: AnsiColor -> Bool
isBright color =
  case color of 
    AnsiColor Bright _ -> True
    _ -> False 

isEmpty :: [a] -> Bool
isEmpty l =
    case l of 
        [] -> True
        _ : _ -> False


main :: IO ()
main = do
  putStrLn $ render $ html_ "title" (h1_ "My Header''''" <> p_ "paragraphic")
  putStrLn $ getInnerString $ ul_ [p_ "hi", p_ "bye"]
  putStrLn $ getInnerString $ ol_ [p_ "hi", p_ "bye"]
