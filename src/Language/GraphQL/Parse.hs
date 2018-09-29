module Language.GraphQL.Parse (document) where

import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.Attoparsec.Text as Atto
import Data.Char (isSpace, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import qualified Data.Scientific as Sci (floatingOrInteger, scientific, toBoundedInteger)
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
        <|> ODNTyped <$> operationType <*> token (option Nothing (Just <$> name) ) <*> maybeVariableDefinitions <*> pure Nothing <*> selectionSet

operationType :: Parser OperationType
operationType = QUERY <$ token "query"
-- opType = QUERY <$ "query" <|> MUTATION <$ "mutation" <|> SUBSCRIPTION <$ "subscription"

maybeVariableDefinitions :: Parser (Maybe VariableDefinitions)
maybeVariableDefinitions =
                Just <$> (token "(" *> variableDefinitions <* token ")")
        <|>     Nothing <$ token ""

variableDefinitions :: Parser VariableDefinitions
variableDefinitions = (:|) <$> variableDefinition <*> many variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition <$> variable <* token ":" <*> type_ <*> option Nothing (Just <$> vdefault)

variable :: Parser Text
variable = token ("$" *> name)

type_ :: Parser Type
type_ =
        TNamed <$> token (name <* "!") <*> pure True
    <|> TNamed <$> token name <*> pure False
    <|> TList <$> (token "[" *> token type_ <* token "]!") <*> pure True
    <|> TList <$> (token "[" *> token type_ <* token "]") <*> pure False

    -- T <$ token ( "Int!" <|> "Int" )

vdefault :: Parser Value
vdefault = token "=" *> token value


value :: Parser Value
value =
            number
        <|> VString <$> token ("\"" *> Atto.takeWhile (/= '"') <* "\"" )
    where
        number = do
            parsed <- scientific
            let interpreted = Sci.floatingOrInteger parsed
            either asFloat asInt interpreted
            where
                asFloat = pure . VFloat
                asInt intv =
                    let maybeInt = (Sci.toBoundedInteger . (flip Sci.scientific 0)) intv
                    in maybe (fail "out of range integer") (pure . VInt) maybeInt


selectionSet :: Parser SelectionSet
selectionSet = token "{" *> many selection <* token "}"

selection :: Parser Selection
selection = Field <$> pure Nothing <*> token name <*> pure Nothing <*> pure Nothing <*> pure Nothing

name :: Parser Text
name = token $ append <$> takeWhile1 isA_z
                    <*> Atto.takeWhile ((||) <$> isDigit <*> isA_z)
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


