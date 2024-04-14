module Generator where

import qualified Convert
import qualified Html.Internal
import qualified Markup as M
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Html (Title)

-- Parses a document to markup, convert to HTML, then render to string
process :: Title -> String -> String
process title =
    Html.Internal.render . Convert.convert title . M.parse_
