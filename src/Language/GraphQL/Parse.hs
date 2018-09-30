module Language.GraphQL.Parse (document) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad (when)
import Data.Attoparsec.Text as Atto
import Data.Char (isSpace, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import qualified Data.Scientific as Sci (floatingOrInteger, scientific, toBoundedInteger)
import Data.Text (Text, append, unpack)
import Language.GraphQL.Syntax

document :: Parser DocumentNode
document = ignored *> definitions

definitions :: Parser (NonEmpty DefinitionNode)
definitions = (:|) <$> definition <*> many definition

definition :: Parser DefinitionNode
definition = DNExecutable <$> token executableDefinition

executableDefinition :: Parser ExecutableDefinitionNode
executableDefinition =
        EDNOperation <$> operationDefinition
    <|> EDNFragment <$> fragmentDefinition


operationDefinition :: Parser OperationDefinitionNode
operationDefinition =
            ODNSelectionSet <$> selectionSet
        <|> ODNTyped <$> operationType <*> token (option Nothing (Just <$> name) ) <*> maybeVariableDefinitions <*> optional directives <*> selectionSet

fragmentDefinition :: Parser FragmentDefinitionNode
fragmentDefinition = FragmentDefinition <$> (token "fragment" *> token name) <*> (token "on" *> token name) <*> optional directives <*> selectionSet

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

vdefault :: Parser Value
vdefault = token "=" *> token value


value :: Parser Value
value =
            numv
        <|> VString <$> token ("\"" *> Atto.takeWhile (/= '"') <* "\"" )
        <|> VBool <$ token "true" <*> pure True
        <|> VBool <$ token "false" <*> pure False
        <|> VNull <$ token "null"
        <|> VEnum <$> name
        <|> VList <$> (token "[" *> many (token value) <* token "]")
        <|> VObject <$> (token "{" *> many entry <* token "}")
    where
        numv = do
            parsed <- scientific
            let interpreted = Sci.floatingOrInteger parsed
            either asFloat asInt interpreted
            where
                asFloat = pure . VFloat
                asInt intv =
                    let maybeInt = (Sci.toBoundedInteger . (flip Sci.scientific 0)) intv
                    in maybe (fail "out of range integer") (pure . VInt) maybeInt
        entry = (,) <$> token name <* token ":" <*> token value


selectionSet :: Parser SelectionSet
selectionSet = token "{" *> ((:|) <$> selection <*> many selection) <* token "}"

selection :: Parser Selection
selection =
        Field <$> optional alias <*> token name <*> optional arguments <*> optional directives <*> optional selectionSet
    <|> FragmentSpread <$> (token "..." *> token fragmentName) <*> optional directives
    <|> InlineFragment <$> (token "..." *> optional (token "on" *> token fragmentName)) <*> optional directives <*> selectionSet

alias :: Parser Text
alias = token name <* token ":"

arguments :: Parser Arguments
arguments = token "(" *> ((:|) <$> argument <*> many argument) <* token ")"

argument :: Parser Argument
argument = Argument <$> token name <*> (token ":" *> token value)

directives :: Parser Directives
directives = (:|) <$> directive <*> many directive

directive :: Parser Directive
directive = Directive <$> ("@" *> token name) <*> optional arguments

name :: Parser Text
name = token $ append <$> takeWhile1 isA_z
                    <*> Atto.takeWhile ((||) <$> isDigit <*> isA_z)
  where
    isA_z =  inClass $ '_' : ['A'..'Z'] <> ['a'..'z']

nameBut :: [Text] -> Parser Text
nameBut disallow = do
    v <- name
    when (elem v disallow) $ fail ("unexpected value" ++ (unpack v))
    pure v

fragmentName :: Parser Text
fragmentName = nameBut ["on"]

token :: Parser a -> Parser a
token t = t <* ignored


ignored :: Parser ()
ignored =
    endOfInput <|>
    do
        c <- peekChar'
        if (isSpace c || c == ',')
            then anyChar *> ignored
            else when (c == '#') $ manyTill anyChar (endOfLine <|> endOfInput) *> ignored



