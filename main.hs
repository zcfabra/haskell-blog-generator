import Html

replicate_ :: Int -> a -> [a]
replicate_ numEls toReplicate = 
    if numEls <= 0
        then []
        else toReplicate : replicate_ (numEls -1) toReplicate


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
    

main :: IO ()
main = do
  putStrLn $ render $ html_ "title" (append_ (h1_ "My Header''''") (p_ "paragraphic"))
  putStrLn $ getInnerString $ ul_ [p_ "hi", p_ "bye"] 
  putStrLn $ getInnerString $ ol_ [p_ "hi", p_ "bye"] 
