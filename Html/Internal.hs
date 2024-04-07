module Html.Internal where


{-
Types:
    newtype <type name> = <constructor> <existing type>
    The first `Html` is the type's representation in the types namespace
    The second `Html` lives in the expressions namespace
    First and second don't actually have to be the same
-}
newtype Html = Html String

newtype Structure = Structure String

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

append_ (Structure a) (Structure b) = Structure (a <> b)

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
