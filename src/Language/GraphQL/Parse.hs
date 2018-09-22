module Language.GraphQL.Parse (document) where

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isSpace, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import Data.Text (Text, append)
import Language.GraphQL.Syntax



-- goals
-- input1 :: Text
-- input1 = "{ amount }"

-- input2 :: Text
-- input2 = "query myQuery { amount }"


-- input3 :: Text
-- input3 = "query { amount currency }"

-- input4 :: Text
-- input4 = "{ amount currency,posting_date }"


output :: Document
output = Document $
            (ExecutableDefinition
                (OpDefExecutableDefinition (
                    SelSetOperationDefinition
                        (SelectionSet [
                            Field Nothing "amount" Nothing Nothing Nothing
                        ])
                    )
                )
            )
            :| []


document :: Parser Document
document = Document <$> (ignored *>token definitions)

definitions :: Parser (NonEmpty Definition)
definitions = (:|) <$> definition <*> pure []

definition :: Parser Definition
definition = ExecutableDefinition <$> executableDefinition

executableDefinition :: Parser ExecDef
executableDefinition = OpDefExecutableDefinition <$> opDef

opDef :: Parser OpDef
opDef =
            SelSetOperationDefinition <$> selSetOpDef
        <|> OpTypeOperationDefinition <$> token opType <*> token (option Nothing (Just <$> name) ) <*> pure Nothing <*> pure Nothing <*> selSetOpDef

opType :: Parser OperationType
opType = QUERY <$ "query" <|> MUTATION <$ "mutation" <|> SUBSCRIPTION <$ "subscription"

selSetOpDef :: Parser SelectionSet
selSetOpDef = SelectionSet <$> (token "{" *> token selections <* token "}")

selections :: Parser [Selection]
selections = (:) <$> selection <*> many selection

selection :: Parser Selection
selection = Field <$> pure Nothing <*> token name <*> pure Nothing <*> pure Nothing <*> pure Nothing

name :: Parser Text
name = token $ append <$> takeWhile1 isA_z
                    <*> Data.Attoparsec.Text.takeWhile ((||) <$> isDigit <*> isA_z)
  where
    isA_z =  inClass $ '_' : ['A'..'Z'] <> ['a'..'z']


token :: Parser a -> Parser a
token t = t <* (ignored <|> endOfInput)


ignored :: Parser ()
ignored =
    do
        c <- peekChar'
        if (isSpace c || c == ',') -- this should be OK, spec has weird def of newline
            then anyChar *> ignored
            else when (c == '#') $ manyTill anyChar endOfLine *> ignored


