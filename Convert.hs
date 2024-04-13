module Convert where

import qualified Markup
import qualified Html.Internal as HI 
import qualified Text.XHtml as HI

convertStructure :: Markup.Structure -> HI.Structure
convertStructure ms = 
    case ms of 
        Markup.Heading level text -> HI.h_ level text
        Markup.Paragraph text -> HI.p_ text
        Markup.UnorderedList list -> HI.ul_ $ map HI.p_ list
        Markup.OrderedList list -> HI.ol_ $ map HI.p_ list
        Markup.CodeBlock lines -> HI.code_ (unlines lines)