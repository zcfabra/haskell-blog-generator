import Html


main :: IO ()
main = do
  putStrLn $ render $ html_ "title" (append_ (h1_ "My Header''''") (p_ "paragraphic"))
  putStrLn $ getInnerString $ ul_ [p_ "hi", p_ "bye"] 
  putStrLn $ getInnerString $ ol_ [p_ "hi", p_ "bye"] 
