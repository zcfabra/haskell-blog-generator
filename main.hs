module Main where
import Text.PrettyPrint (TextDetails(Str))
import Control.Monad.RWS.Lazy (MonadState(get))
import Data.Array.Base (STUArray(STUArray))

{-
    Partial application:
        el takes 2 args, so what is returned from these 'tag' functions here
        is acually a partially applied function
-}

html_ title content =
    Html (
        el "html" (
            el "head" (el "title" title) <> el "body" (getInnerString content)
        )
    ) 

body_  = Structure . el "body"

title_ = Structure . el "title"

head_ = Structure . el "head"

p_  = Structure . el "p"
h1_  = Structure . el "h1"



el tag content = "<" <> tag <> ">" <> content <> "<" <> tag <> "/>"

{-
Types:
    newtype <type name> = <constructor> <existing type>
    The first `Html` is the type's representation in the types namespace
    The second `Html` lives in the expressions namespace
    First and second don't actually have to be the same
-}
newtype Html = Html String
newtype Structure = Structure String

getInnerString :: Structure  -> String
getInnerString struct = 
    case struct of 
        Structure str -> str

getInnerHtmlText :: Html -> String
getInnerHtmlText html =
    case html of 
        Html text -> text

render = getInnerHtmlText 

append_ (Structure a) (Structure b) =  Structure (a <> b)

main :: IO()
main = do
    putStrLn $ render $ html_ "title" (append_  (h1_  "My Header") (p_  "paragraphic"))
