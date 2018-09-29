module Language.GraphQL.Parse (document) where

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isSpace, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import Data.Text (Text, append)
import Language.GraphQL.Syntax

document :: Parser DocumentNode
-- document = (ignored *>token definitions)
document = definitions

definitions :: Parser (NonEmpty DefinitionNode)
definitions = (:|) <$> definition <*> pure []

definition :: Parser DefinitionNode
definition = DNExecutable <$> executableDefinition

executableDefinition :: Parser ExecutableDefinitionNode
executableDefinition = EDNOperation <$> operationDefinition -- <$> opDef

operationDefinition :: Parser OperationDefinitionNode
operationDefinition =
            ODNSelectionSet <$> selectionSet
        <|> ODNTyped <$> operationType <*> token (option Nothing (Just <$> name) ) <*> pure Nothing <*> pure Nothing <*> selectionSet

operationType :: Parser OperationType
operationType = OT <$ token "query"
-- opType = QUERY <$ "query" <|> MUTATION <$ "mutation" <|> SUBSCRIPTION <$ "subscription"

selectionSet :: Parser SelectionSet
selectionSet = SS <$ (token "{" *> many name <* token "}")

--SS:: arser [Selectiomany anyChar = (:) <$> selection <*> many selection

-- selection :: Parser Selection
-- selection = Field <$> pure Nothing <*> token name <*> pure Nothing <*> pure Nothing <*> pure Nothing

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


