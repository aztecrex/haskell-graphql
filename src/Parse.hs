module Parse where

import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Int (Int32)
import Data.List.NonEmpty
import Data.Text (Text)

-----------------------
-- AST - this is straight out of the spec

data Document where
    Document :: (NonEmpty Definition) -> Document
    deriving (Show, Eq)

data Definition where
    ExecutableDefinition :: ExecutableDefinition -> Definition
    TypeSystemDefinition :: Definition
    TypeSystemExtension :: Definition
    deriving (Show, Eq)

data TypeSysDef where


data ExecutableDefinition where
    OpDefExecutableDefinition :: OperationDefinition -> ExecutableDefinition
    FragmentDefinition :: FragmentName -> TypeCondition -> Maybe Directives -> ExecutableDefinition
    deriving (Show, Eq)

data OperationDefinition where
    SelSetOperationDefinition :: SelectionSet -> OperationDefinition
    OpTypeOperationDefinition :: OperationType -> Maybe Text -> Maybe VariableDefinitions -> Maybe Directives -> SelectionSet -> OperationDefinition
    deriving (Show, Eq)

data OperationType where
    QUERY :: OperationType
    MUTATION :: OperationType
    SUBSCRIPTION :: OperationType
    deriving (Show, Eq)

data VariableDefinitions where
    VariableDefinitions :: [VariableDefinition] -> VariableDefinitions
    deriving (Show, Eq)

data VariableDefinition where
    VariableDefinition :: Type -> Maybe DefaultValue -> VariableDefinition
    deriving (Show, Eq)

data Variable where
    Variable :: Text -> Variable
    deriving (Show, Eq)

data DefaultValue where
    DefaultValue :: Text -> DefaultValue
    deriving (Show, Eq)

data Directives where
    Directives :: [Directive] -> Directives
    deriving (Show, Eq)

data Directive where
    Directive :: Text -> Arguments -> Directive
    deriving (Show, Eq)


data SelectionSet where
    SelectionSet :: [Selection] -> SelectionSet
    deriving (Show, Eq)

data Selection where
    Field :: Maybe Alias -> Text -> Maybe Arguments -> Maybe Directives -> Maybe SelectionSet -> Selection
    FragmentSpread :: FragmentName -> Maybe Directives -> Selection
    InlineFragment :: Maybe TypeCondition -> Maybe Directives -> SelectionSet -> Selection
    deriving (Show, Eq)

data FragmentName where
    FragmentName :: Text -> FragmentName
    deriving (Show, Eq)

data TypeCondition where
    TypeCondition :: Text -> TypeCondition
    deriving (Show, Eq)

data Alias where
    Alias :: Text -> Alias
    deriving (Show, Eq)

data Arguments where
    Arguments :: [Argument] -> Arguments
    deriving (Show, Eq)

data Argument where
    Argument :: Text -> Value -> Argument
    deriving (Show, Eq)

data ObjectField where
    ObjectField :: Text -> Value -> ObjectField
    deriving (Show, Eq)

data Value where
    VariableValue :: Variable -> Value
    IntValue :: Int32 -> Value
    FloatValue :: Double -> Value
    StringValue :: Text -> Value
    BooleanValue :: Bool -> Value
    NullValue :: Value
    EnumValue :: Text -> Value
    ListValue :: [Value] -> Value
    ObjectValue :: [ObjectField] -> Value
    deriving (Show, Eq)

data Type where
    NamedType :: Text -> Type
    ListType :: [Type] -> Type
    NonNullNamedType :: Text -> Type
    NonNullListType :: [Type] -> Type
    deriving (Show, Eq)



-----------------------
-- parse

-- document :: Parser Document
-- document = ignored *> (Document <$> ( many1 (token definition) "done")) <* ignored

-- definition :: Parser Definition
-- definition =

    -- definition :: Parser Text
    -- definition = "definition"



token :: Parser a -> Parser a
token t = t <* ignored


ignored :: Parser ()
ignored = do
    c <- peekChar'
    if (isSpace c || c == ',') -- this should be OK, spec has weird def of newline
        then anyChar *> ignored
        else when (c == '#') $ manyTill anyChar endOfLine *> ignored


x :: Text
x = ""

p :: Parser Char
p = ignored *> anyChar

