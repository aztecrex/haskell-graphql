module Parse where

import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text (Text)

data Document = Document [Text]

-- document :: Parser Document
-- document = ignored *> many1 definition

-- definition :: Parser Text
-- definition =

ignored :: Parser ()
ignored = do
    c <- peekChar'
    if (isSpace c || c == ',')  -- this should be OK, spec has weird def of newline
        then anyChar *> ignored
        else when (c == '#') $ manyTill anyChar endOfLine *> ignored


x :: Text
x = ""

p :: Parser Char
p = ignored *> anyChar

