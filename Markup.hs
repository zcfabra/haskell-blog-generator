module Markup(
    Structure(..), 
    Document,
    parse_
) where
import Numeric.Natural
import Html (getInnerString)
import Data.Maybe (maybeToList)

type Document = [Structure]
data Structure =
    Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving Show

parse_ :: String -> Document 
parse_ = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts = 
    -- Pass new lines in reverse order because of the prepend vs append stuff 
    case txts of 
        [] -> maybeToList context

        -- H1 case
        ('*' : ' ' : line) : rest -> maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
        -- Unodereded list
        ('-' : ' ' : line) : rest -> 
            case context of
                Just(UnorderedList list) -> parseLines (Just (UnorderedList (list <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
        -- Code Block
        ('>' : ' ' : line) : rest -> 
            case context of 
                Just (CodeBlock code) -> parseLines (Just (CodeBlock( code <> [line]))) rest 
                _ -> maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)
        -- Ordered List
        ('#' : ' ' : line) : rest ->
            case context of
                Just (OrderedList list) -> parseLines (Just (OrderedList (list <> [trim line]))) rest
                _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)



        -- Para. case
        currentLine : rest -> 
            let line = trim currentLine in
                if line == ""
                    {-- maybe fn:
                        arg1 is a value to return if Nothing,
                        arg2 is a function to apply to Just case,
                        arg3 is the value on which we pattern match --}
                    then maybe id (:) context (parseLines Nothing rest)
                    else
                        case context of 
                            Just (Paragraph paragraph) ->
                                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
                            Nothing ->
                                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim = unwords . words
