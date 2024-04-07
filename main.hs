import Html


main :: IO ()
main = do
  putStrLn $ render $ html_ "title" (append_ (h1_ "My Header''''") (p_ "paragraphic"))
