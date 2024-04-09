module Markup(
    Structure(..), 
    Document
) where
import Numeric.Natural
type Document = [Structure]
data Structure =
    Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]

parse :: String -> Document 
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts = 
    -- Pass new lines in reverse order because of the prepend vs append stuff 
    let para = Paragraph(unlines (reverse currentParagraph))
    in case txts of 
        [] -> [para]
        currentLine : rest -> 
            if trim currentLine == ""
                then
                    para : parseLines [] rest
                else
                    parseLines (currentLine : currentParagraph) rest


trim = unwords . words
