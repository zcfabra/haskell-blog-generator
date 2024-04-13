module Html.Internal where
import GHC.Natural (Natural)


{-
Types:
    newtype <type name> = <constructor> <existing type>
    The first `Html` is the type's representation in the types namespace
    The second `Html` lives in the expressions namespace
    First and second don't actually have to be the same
-}
newtype Html = Html String

newtype Structure = Structure String
instance Semigroup Structure where
    (<>) :: Structure -> Structure -> Structure
    (<>) a b = Structure (getInnerString a <> getInnerString b)

newtype Title = Title String

html_ :: String -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        (el "head" (el "title" $ escape title) <> el "body" (getInnerString content))
    )

body_ = Structure . el "body"

title_ = Structure . el "title"

head_ = Structure . el "head"

p_ = Structure . el "p" . escape

code_ = Structure . el "pre" . escape

h_ :: Natural -> String -> Structure
h_ level = 
    let tagText = "h" <> show level
    in Structure . el tagText . escape

h1_ = Structure . el "h1" . escape


{-
    Partial application:
        el takes 2 args, so what is returned from these 'tag' functions here
        is acually a partially applied function
-}
el tag content = "<" <> tag <> ">" <> content <> "<" <> tag <> "/>"

getInnerString :: Structure -> String
getInnerString struct =
  case struct of
    Structure str -> str

getInnerHtmlText :: Html -> String
getInnerHtmlText html =
  case html of
    Html text -> text

render = getInnerHtmlText


escape :: String -> String
escape =
  let escChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escChar

-- What this does is apply the escape char fn to each char in a list
-- then concat it all together: chars --> map --> concat


li_ :: String -> Structure 
li_ = Structure . el "li"


-- listify :: [Structure] -> Structure 
-- listify els = 
--     customFold (map (li_ .  getInnerString) els )

-- ul_ :: [Structure] -> Structure 
-- ul_ els = 
--     let inner = listify els
--     in Structure $ el "ul" (getInnerString inner)

-- ol_ :: [Structure] -> Structure
-- ol_ els =  
--     let inner = listify els
--     in Structure $ el "ol" (getInnerString inner)

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getInnerString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getInnerString)


{--
data vs newtypes

data <Type-name> <type-args>
= <Data-constructor> <types>
| <Data-constructor> <types>
| <Data-constructor> <types>
| ...

newtypes is a typesafe alisa
data is a composite type
subtle differences from newtypes:
1. data type constructors can deal in multiple args, newtypes can only take one
2. can have alternative types with |, but newtypes cannot have alternatives
--}
