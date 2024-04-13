module Convert where

import qualified Markup
import qualified Html.Internal as HI 

convertStructure :: Markup.Structure -> HI.Structure
convertStructure ms = 
    case ms of 
        Markup.Heading level text -> HI.h_ level text
        Markup.Paragraph text -> HI.p_ text
        Markup.UnorderedList list -> HI.ul_ $ map HI.p_ list
        Markup.OrderedList list -> HI.ol_ $ map HI.p_ list
        Markup.CodeBlock lines -> HI.code_ (unlines lines)

concatStructures :: [HI.Structure] -> HI.Structure
concatStructures list =  
    case list of 
        [] -> HI.empty_
        ms : rest -> ms <> concatStructures rest



{-- Monoids

Imagine an 'identity' element that satisifies the following laws:
x <> <identity> = x
<identity> <> x = x

The identity argument can be concated with another argument and just 
give you back that other argument. For strings, the empty string gives you this

For numbers, 0 seems like a monoid (i.e. 10 + 0 = 10)
However, it is not actually a monoid because 10 * 1 = 10 also has a similar effect
There can only be one monoid instance per type
So, there are newtypes Sum and Product over number to account for this

Foldable:

Foldable is a class that describes a data structure that can be collapsed into
a single value

fold :: (Foldable t, Monoid m) => t m -> m 
For example, a list (Foldable) of ints (Monoid)


--}

convert :: HI.Title -> Markup.Document -> HI.Html
convert title = HI.html_ title . foldMap convertStructure 
