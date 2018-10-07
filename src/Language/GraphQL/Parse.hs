module Language.GraphQL.Parse (document) where

import Control.Applicative ((<|>), many, optional)
import Control.Monad (when)
import Data.Attoparsec.Text as Atto
import Data.Char (isSpace, isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ((<>))
import qualified Data.Scientific as Sci (floatingOrInteger, scientific, toBoundedInteger)
import Data.Text as T (Text, append, unpack, concat, singleton, lines, length, takeWhile, intercalate, drop, all)
import Language.GraphQL.Syntax

document :: Parser DocumentNode
document = ignored *> definitions

definitions :: Parser (NonEmpty DefinitionNode)
definitions = (:|) <$> definition <*> many definition

definition :: Parser DefinitionNode
definition =
        DNExecutable <$> token executableDefinition
    <|> DNTypeSystem <$> token typeSystemDefinition
    <|> DNTypeSystemExtension <$> token typeSystemExtension

executableDefinition :: Parser ExecutableDefinitionNode
executableDefinition =
        EDNOperation <$> operationDefinition
    <|> EDNFragment <$> fragmentDefinition

typeSystemDefinition :: Parser TypeSystemDefinitionNode
typeSystemDefinition =
        TSDNRoots <$> (token "schema" *> token rootOperationTypes)
    <|> TSDNDirective <$> directiveDefinition
    <|> TSDNType <$> typeDefinition

typeSystemExtension :: Parser TypeSystemExtensionNode
typeSystemExtension =
        TSENSchema <$> schemaExtension

schemaExtension :: Parser SchemaExtensionNode
schemaExtension =
        SchemaExtendRoots <$ (token "extend" *> token "schema") <*> optional directives <*> rootOperationTypes
    <|> SchemaExtendDirectives <$> (token "extend" *> token "schema" *> directives)


typeExtension :: Parser TypeSystemExtensionNode
typeExtension = undefined


directiveDefinition :: Parser DirectiveDefinitionNode
directiveDefinition = DDNDefinition <$> (token "directive" *> token "@" *> token name) <*> optional argumentsDefinition <*> (token "on" *> token directiveLocation)

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = token "(" *> ((:|) <$> token inputValueDefinition <*> many inputValueDefinition) <* token ")"

typeDefinition :: Parser TypeDefinitionNode
typeDefinition =
        scalarTypeDef
    <|> enumTypeDef
    <|> objectTypeDef
    <|> interfaceTypeDef
    <|> unionTypeDef
    <|> inputTypeDef

scalarTypeDef :: Parser TypeDefinitionNode
scalarTypeDef = TDNScalar <$> optional description <*> (token "scalar" *> token name) <*> optional (token directives)

enumTypeDef :: Parser TypeDefinitionNode
enumTypeDef = TDNEnum <$>
        optional description
    <*> (token "enum"
    *>  token name)
    <*> optional (token directives)
    <*> optional (token "{" *> ((:|) <$> enumValueDef <*> many enumValueDef) <* token "}")

enumValueDef :: Parser EnumValueDefNode
enumValueDef = EnumValueDef <$>
        optional description
    <*> token (nameBut ["null", "true", "false"])
    <*> optional (token directives)

objectTypeDef :: Parser TypeDefinitionNode
objectTypeDef = TDNObject <$>
        (optional description)
    <*> (token "type"
    *> token name)
    <*> optional implementsInterfaces
    <*> optional (token directives)
    <*> optional fieldsDefinition


implementsInterfaces :: Parser (NonEmpty Text)
implementsInterfaces = token "implements" *> optional (token "&") *> ((:|) <$> token name <*> many (token name))

interfaceTypeDef :: Parser TypeDefinitionNode
interfaceTypeDef = TDNInterface <$>
        (optional description)
    <*> (token "interface"
    *>  token name)
    <*> optional (token directives)
    <*> optional fieldsDefinition


fieldsDefinition :: Parser (NonEmpty FieldDefinitionNode)
fieldsDefinition = token "{" *> ((:|) <$> token fieldDefinition <*> many (token fieldDefinition)) <* token "}"

fieldDefinition :: Parser FieldDefinitionNode
fieldDefinition = FieldDefinition <$>
        optional description
    <*> token name
    <*> optional argumentsDefinition
    <*> (token ":" *> token type_)
    <*> optional directives

unionTypeDef :: Parser TypeDefinitionNode
unionTypeDef = TDNUnion <$>
        (optional description)
    <*> (token "union"
    *>  token name)
    <*>  optional (token directives)
    <*>  optional unionMembers


unionMembers :: Parser (NonEmpty Text)
unionMembers = optional (token "|") *> ((:|) <$> token name <*> many (token "|" *> token name))


inputTypeDef :: Parser TypeDefinitionNode
inputTypeDef = TDNInput <$>
        (optional description)
    <*> (token "input"
    *>  token name)
    <*> optional (token directives)
    <*> optional inputFieldsDefinition


inputFieldsDefinition :: Parser (NonEmpty InputValueDefinitionNode)
inputFieldsDefinition = token "{" *> ((:|) <$> inputValueDefinition <*> many inputValueDefinition) <* token "}"

inputValueDefinition :: Parser InputValueDefinitionNode
inputValueDefinition = IVDN <$>
            optional (token (normalString <|> blockString))
        <*> token name
        <* token ":"
        <*> token type_
        <*> optional (token vdefault)
        <*> optional directives

directiveLocation :: Parser DirectiveLocation
directiveLocation =
        token "QUERY" *> pure DL_QUERY
    <|> token "MUTATION" *> pure DL_MUTATION
    <|> token "SUBSCRIPTION" *> pure DL_SUBSCRIPTION
    <|> token "FIELD_DEFINITION" *> pure DL_FIELD_DEFINITION
    <|> token "FIELD" *> pure DL_FIELD
    <|> token "FRAGMENT_DEFINITION" *> pure DL_FRAGMENT_DEFINITION
    <|> token "FRAGMENT_SPREAD" *> pure DL_FRAGMENT_SPREAD
    <|> token "INLINE_FRAGMENT" *> pure DL_INLINE_FRAGMENT
    <|> token "SCHEMA" *> pure DL_SCHEMA
    <|> token "SCALAR" *> pure DL_SCALAR
    <|> token "OBJECT" *> pure DL_OBJECT
    <|> token "ARGUMENT_DEFINITION" *> pure DL_ARGUMENT_DEFINITION
    <|> token "INTERFACE" *> pure DL_INTERFACE
    <|> token "UNION" *> pure DL_UNION
    <|> token "ENUM_VALUE" *> pure DL_ENUM_VALUE
    <|> token "ENUM" *> pure DL_ENUM
    <|> token "INPUT_OBJECT" *> pure DL_INPUT_OBJECT
    <|> token "INPUT_FIELD_DEFINITION" *> pure DL_INPUT_FIELD_DEFINITION

rootOperationTypes :: Parser RootOperationTypeDefinitionsNode
rootOperationTypes = token "{" *> ((:|) <$> token rootOperationType <*> many (token rootOperationType)) <* token "}"

rootOperationType :: Parser RootOperationTypeDefinitionNode
rootOperationType = ROTDNDefinition <$> token operationType <*> (token ":" *> token name )

operationDefinition :: Parser OperationDefinitionNode
operationDefinition =
            ODNSelectionSet <$> selectionSet
        <|> ODNTyped <$> operationType <*> token (option Nothing (Just <$> name) ) <*> maybeVariableDefinitions <*> optional directives <*> selectionSet

fragmentDefinition :: Parser FragmentDefinitionNode
fragmentDefinition = FragmentDefinition <$> (token "fragment" *> token name) <*> (token "on" *> token name) <*> optional directives <*> selectionSet

operationType :: Parser OperationType
operationType =
        QUERY <$ token "query"
    <|> MUTATION <$ token "mutation"
    <|> SUBSCRIPTION <$ token "subscription"
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
        <|> VString <$> token blockString
        <|> VString <$> token normalString
        <|> VBool <$ token "true" <*> pure True
        <|> VBool <$ token "false" <*> pure False
        <|> VNull <$ token "null"
        <|> VEnum <$> name -- order important, this tried after bool and null
        <|> VList <$> (token "[" *> many (token value) <* token "]")
        <|> VObject <$> (token "{" *> many entry <* token "}")
        <|> VVariable <$> variable
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


description :: Parser Text
description = token (blockString <|> normalString)

blockString :: Parser Text
blockString = "\"\"\"" *> ((fixup . T.concat)<$> many stok) <* "\"\"\""
    where
        stok = schars <|> check
        schars = Atto.takeWhile1 (\c -> c >= '\x0009' && c /= '\\' && c /= '\"')
        check =
                "\\\"\"\"" *> pure "\"\"\""
            <|> "\\"
        fixup v =
            let ls = T.lines v
                burn = foldl indent (maxBound::Int) ls
                indent a x =
                    let spaces = T.length . T.takeWhile isSpace $ x
                    in if spaces == T.length x then a else min a spaces
                indented = T.drop burn <$> ls
                rtrimmed = reverse . dropWhile blank . reverse $ indented
                ltrimmed = dropWhile blank rtrimmed
                blank = T.all isSpace
            in T.intercalate "\n" ltrimmed


normalString :: Parser Text
normalString = "\"" *> (T.concat <$> many stok)  <* "\""
    where
        stok = schars <|> escape
        schars = Atto.takeWhile1 schar
        schar c = c >= '\x0009' && (not (elem c ("\\\n\r\"" :: [Char])))
        escape = "\\" *> (
                "b" *> pure "\b"
            <|> "\\" *> pure "\\"
            <|> "n" *> pure "\n"
            <|> "r" *> pure "\r"
            <|> "/" *> pure "/"
            <|> "f" *> pure "\f"
            <|> "t" *> pure "\t"
            <|> "\"" *> pure "\""
            <|> "u" *> (hchar <$> (Atto.take 4 >>= validHex))
            )
        hchar cs = singleton . toEnum $ foldl (\a x -> a * 16 + hv x) 0 (unpack cs)
        hv c    | elem c ['0'..'9'] = fromEnum c - fromEnum '0'
                | elem c ['a' .. 'f'] = 10 + fromEnum c - fromEnum 'a'
                | elem c ['A' .. 'F'] = 10 + fromEnum c - fromEnum 'A'
                | otherwise = undefined -- do not let this happen
        validHex :: Text -> Parser Text
        validHex v = if T.all (flip elem hex) v then pure v else fail "not valid hex"
        hex = ['a'..'f'] <> ['A'..'F'] <> ['0'..'9']

ignored :: Parser ()
ignored =
    endOfInput <|>
    do
        c <- peekChar'
        if (isSpace c || c == ',')
            then anyChar *> ignored
            else when (c == '#') $ manyTill anyChar (endOfLine <|> endOfInput) *> ignored



