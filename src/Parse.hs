module Parse where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Attoparsec.Text
import Data.Char (isSpace, isDigit)
import Data.Int (Int32)
import Data.List.NonEmpty
import Data.Monoid ((<>))
import Data.Text (Text, append)

-----------------------
-- AST - this is straight out of the spec

data Document where
    Document :: (NonEmpty Definition) -> Document
    deriving (Show, Eq)

data Definition where
    ExecutableDefinition :: ExecDef -> Definition
    TypeSystemDefinition :: TypeSysDef -> Definition
    TypeSystemExtension :: TypeSysExt -> Definition
    deriving (Show, Eq)

data TypeSysDef where
    SchemaDefinition :: Maybe Directives -> NonEmpty RootOperationTypeDefinition -> TypeSysDef
    TypeDefinition :: TypeSysDef
    DirectiveDefinition :: TypeSysDef
    deriving (Show, Eq)

data TypeDef where
    ScalarTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> TypeDef
    ObjectTypeDefinition :: Text -> Maybe ImplemntsIfcs -> Maybe Directives -> Maybe FieldsDef -> TypeDef
    InterfaceTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe FieldsDef -> TypeDef
    UnionTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe UnionMemTypes -> TypeDef
    EnumTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe EnumValsDef -> TypeDef
    InputObjectTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe InputFieldsDefinition -> TypeDef
    deriving (Show, Eq)

data InputFieldsDefinition where
    InputFieldsDefinition :: NonEmpty InputValueDefinition  -> InputFieldsDefinition
    deriving (Show, Eq)

data EnumValsDef where
    EnumValuesDefinition :: NonEmpty EnumValDef -> EnumValsDef
    deriving (Show, Eq)

data EnumValDef where
    EnumValueDef :: Maybe Text -> Text -> Maybe Directives -> EnumValDef
    deriving (Show, Eq)



data ImplemntsIfcs where
    ImplementsInterfaces :: NonEmpty Text -> ImplemntsIfcs
    deriving (Show, Eq)

data FieldsDef where
    FieldsDefinition :: NonEmpty FieldDef -> FieldsDef
    deriving (Show, Eq)

data FieldDef where
    FieldDefinition :: Maybe Text -> Text -> Maybe ArgsDef -> Type -> Maybe Directives -> FieldDef
    deriving (Show, Eq)

data UnionMemTypes where
    UnionMemberTypes :: NonEmpty Text -> UnionMemTypes
    deriving (Show, Eq)

data ArgsDef where
    ArgumentsDefinition :: NonEmpty InputValueDefinition -> ArgsDef
    deriving (Show, Eq)

data InputValueDefinition where
    InputValueDefinition :: Maybe Text -> Text -> Type -> Maybe DefaultValue -> Maybe Directives -> InputValueDefinition
    deriving (Show, Eq)



data RootOperationTypeDefinition where
    RootOperationTypeDefinition :: Text -> OperationType -> RootOperationTypeDefinition
    deriving (Show, Eq)


data TypeSysExt where
    SchemaExtension :: Maybe Directives -> [OperationTypeDefinition] -> TypeSysExt
    TypeExtension :: TypeExt -> TypeSysExt
    deriving (Show, Eq)

data OperationTypeDefinition where
    OperationTypeDefinition :: OperationType -> Text -> OperationTypeDefinition
    deriving (Show, Eq)

data TypeExt where
    ScalarTypeExtension :: Text -> Maybe Directives -> TypeExt
    ObjectTypeExtension :: Text -> Maybe ImplemntsIfcs -> Maybe Directives -> Maybe FieldsDef -> TypeExt
    InterfaceTypeExtension :: Text -> Maybe Directives -> Maybe FieldsDef -> TypeExt
    UnionTypeExtension :: Text -> Maybe Directives -> Maybe UnionMemTypes -> TypeExt
    EnumTypeExtension :: Text -> Maybe Directives -> EnumValsDef -> TypeExt
    InputObjectTypeExtension :: Text -> Maybe Directives -> InputFieldsDefinition -> TypeExt
    deriving (Show, Eq)

data ExecDef where
    OpDefExecutableDefinition :: OpDef -> ExecDef
    FragmentDefinition :: FragmentName -> TypeCondition -> Maybe Directives -> ExecDef
    deriving (Show, Eq)

data OpDef where
    SelSetOperationDefinition :: SelectionSet -> OpDef
    OpTypeOperationDefinition :: OperationType -> Maybe Text -> Maybe VariableDefinitions -> Maybe Directives -> SelectionSet -> OpDef
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
    Directives :: NonEmpty Directive -> Directives
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
    ListType :: Type -> Type
    NonNullNamedType :: Text -> Type
    NonNullListType :: Type -> Type
    deriving (Show, Eq)



-----------------------
-- parse


-- goals
input1 :: Text
input1 = "{ amount }"

input2 :: Text
input2 = "query myQuery { amount }"


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

-- SelSetOperationDefinition :: SelectionSet -> OpDef
-- OpTypeOperationDefinition :: OperationType -> Maybe Text -> Maybe VariableDefinitions -> Maybe Directives -> SelectionSet -> OpDef



opDef :: Parser OpDef
opDef =
            SelSetOperationDefinition <$> selSetOpDef
        <|> OpTypeOperationDefinition <$> token opType <*> token (option Nothing (Just <$> name) ) <*> pure Nothing <*> pure Nothing <*> selSetOpDef

opType :: Parser OperationType
opType = QUERY <$ "query" <|> MUTATION <$ "mutation" <|> SUBSCRIPTION <$ "subscription"

selSetOpDef :: Parser SelectionSet
selSetOpDef = SelectionSet <$> (token "{" *> token selections <* token "}")

selections :: Parser [Selection]
selections = (:) <$> selection <*> pure []

selection :: Parser Selection
selection = Field <$> pure Nothing <*> token name <*> pure Nothing <*> pure Nothing <*> pure Nothing

name :: Parser Text
name = token $ append <$> takeWhile1 isA_z
                    <*> Data.Attoparsec.Text.takeWhile ((||) <$> isDigit <*> isA_z)
  where
    isA_z =  inClass $ '_' : ['A'..'Z'] <> ['a'..'z']


token :: Parser a -> Parser a
token t = t <* ignored


ignored :: Parser ()
ignored =
    endOfInput <|>
    do
        c <- peekChar'
        if (isSpace c || c == ',') -- this should be OK, spec has weird def of newline
            then anyChar *> ignored
            else when (c == '#') $ manyTill anyChar endOfLine *> ignored


x :: Text
x = ""

p :: Parser Char
p = ignored *> anyChar

