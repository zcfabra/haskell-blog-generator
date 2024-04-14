{-# LANGUAGE LambdaCase #-}
import Html
import Data.Maybe (listToMaybe)
import System.Directory.Internal.Prelude (getArgs)
import Generator (process)
import System.Directory (doesFileExist)


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
    -- Want to handle many cases including
    -- 1. Empty args
    -- 2. Input and output file
    getArgs >>= 
        \case
            [] -> 
                getContents >>= \contents -> 
                    putStrLn (process "Untitled" contents)
            [input, output] -> 
                doesFileExist input >>= \exists ->
                    if exists
                        then
                            readFile input >>= \contents -> 
                                writeFile output (process input contents)
                        else 
                            putStrLn $ "Could not find input file " <> input
            _ -> print "Idk man I don't think you're using this thing right"
            
    
