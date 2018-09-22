{-# LANGUAGE DeriveDataTypeable #-}
module Language.GraphQL.Syntax where

--
import Data.Data (Data)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics

data Document where
    Document :: (NonEmpty Definition) -> Document
    deriving (Show, Eq, Generic, Data)

data Definition where
    ExecutableDefinition :: ExecDef -> Definition
    TypeSystemDefinition :: TypeSysDef -> Definition
    TypeSystemExtension :: TypeSysExt -> Definition
    deriving (Show, Eq, Generic, Data)

data ExecDef where
    OpDefExecutableDefinition :: OpDef -> ExecDef
    FragmentDefinition :: FragmentName -> TypeCondition -> Maybe Directives -> ExecDef
    deriving (Show, Eq, Generic, Data)

data TypeSysDef where
    SchemaDefinition :: Maybe Directives -> NonEmpty RootOperationTypeDefinition -> TypeSysDef
    TypeDefinition :: TypeSysDef
    DirectiveDefinition :: TypeSysDef
    deriving (Show, Eq, Generic, Data)

data TypeDef where
    ScalarTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> TypeDef
    ObjectTypeDefinition :: Text -> Maybe ImplemntsIfcs -> Maybe Directives -> Maybe FieldsDef -> TypeDef
    InterfaceTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe FieldsDef -> TypeDef
    UnionTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe UnionMemTypes -> TypeDef
    EnumTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe EnumValsDef -> TypeDef
    InputObjectTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe InputFieldsDefinition -> TypeDef
    deriving (Show, Eq, Generic, Data)

data InputFieldsDefinition where
    InputFieldsDefinition :: NonEmpty InputValueDefinition  -> InputFieldsDefinition
    deriving (Show, Eq, Generic, Data)

data EnumValsDef where
    EnumValuesDefinition :: NonEmpty EnumValDef -> EnumValsDef
    deriving (Show, Eq, Generic, Data)

data EnumValDef where
    EnumValueDef :: Maybe Text -> Text -> Maybe Directives -> EnumValDef
    deriving (Show, Eq, Generic, Data)



data ImplemntsIfcs where
    ImplementsInterfaces :: NonEmpty Text -> ImplemntsIfcs
    deriving (Show, Eq, Generic, Data)

data FieldsDef where
    FieldsDefinition :: NonEmpty FieldDef -> FieldsDef
    deriving (Show, Eq, Generic, Data)

data FieldDef where
    FieldDefinition :: Maybe Text -> Text -> Maybe ArgsDef -> Type -> Maybe Directives -> FieldDef
    deriving (Show, Eq, Generic, Data)

data UnionMemTypes where
    UnionMemberTypes :: NonEmpty Text -> UnionMemTypes
    deriving (Show, Eq, Generic, Data)

data ArgsDef where
    ArgumentsDefinition :: NonEmpty InputValueDefinition -> ArgsDef
    deriving (Show, Eq, Generic, Data)

data InputValueDefinition where
    InputValueDefinition :: Maybe Text -> Text -> Type -> Maybe DefaultValue -> Maybe Directives -> InputValueDefinition
    deriving (Show, Eq, Generic, Data)



data RootOperationTypeDefinition where
    RootOperationTypeDefinition :: Text -> OperationType -> RootOperationTypeDefinition
    deriving (Show, Eq, Generic, Data)


data TypeSysExt where
    SchemaExtension :: Maybe Directives -> [OperationTypeDefinition] -> TypeSysExt
    TypeExtension :: TypeExt -> TypeSysExt
    deriving (Show, Eq, Generic, Data)

data OperationTypeDefinition where
    OperationTypeDefinition :: OperationType -> Text -> OperationTypeDefinition
    deriving (Show, Eq, Generic, Data)

data TypeExt where
    ScalarTypeExtension :: Text -> Maybe Directives -> TypeExt
    ObjectTypeExtension :: Text -> Maybe ImplemntsIfcs -> Maybe Directives -> Maybe FieldsDef -> TypeExt
    InterfaceTypeExtension :: Text -> Maybe Directives -> Maybe FieldsDef -> TypeExt
    UnionTypeExtension :: Text -> Maybe Directives -> Maybe UnionMemTypes -> TypeExt
    EnumTypeExtension :: Text -> Maybe Directives -> EnumValsDef -> TypeExt
    InputObjectTypeExtension :: Text -> Maybe Directives -> InputFieldsDefinition -> TypeExt
    deriving (Show, Eq, Generic, Data)

data OpDef where
    SelSetOperationDefinition :: SelectionSet -> OpDef
    OpTypeOperationDefinition :: OperationType -> Maybe Text -> Maybe VariableDefinitions -> Maybe Directives -> SelectionSet -> OpDef
    deriving (Show, Eq, Generic, Data)

data OperationType where
    QUERY :: OperationType
    MUTATION :: OperationType
    SUBSCRIPTION :: OperationType
    deriving (Show, Eq, Generic, Data)

data VariableDefinitions where
    VariableDefinitions :: [VariableDefinition] -> VariableDefinitions
    deriving (Show, Eq, Generic, Data)

data VariableDefinition where
    VariableDefinition :: Type -> Maybe DefaultValue -> VariableDefinition
    deriving (Show, Eq, Generic, Data)

data Variable where
    Variable :: Text -> Variable
    deriving (Show, Eq, Generic, Data)

data DefaultValue where
    DefaultValue :: Text -> DefaultValue
    deriving (Show, Eq, Generic, Data)

data Directives where
    Directives :: NonEmpty Directive -> Directives
    deriving (Show, Eq, Generic, Data)

data Directive where
    Directive :: Text -> Arguments -> Directive
    deriving (Show, Eq, Generic, Data)


data SelectionSet where
    SelectionSet :: [Selection] -> SelectionSet
    deriving (Show, Eq, Generic, Data)

data Selection where
    Field :: Maybe Alias -> Text -> Maybe Arguments -> Maybe Directives -> Maybe SelectionSet -> Selection
    FragmentSpread :: FragmentName -> Maybe Directives -> Selection
    InlineFragment :: Maybe TypeCondition -> Maybe Directives -> SelectionSet -> Selection
    deriving (Show, Eq, Generic, Data)

data FragmentName where
    FragmentName :: Text -> FragmentName
    deriving (Show, Eq, Generic, Data)

data TypeCondition where
    TypeCondition :: Text -> TypeCondition
    deriving (Show, Eq, Generic, Data)

data Alias where
    Alias :: Text -> Alias
    deriving (Show, Eq, Generic, Data)

data Arguments where
    Arguments :: [Argument] -> Arguments
    deriving (Show, Eq, Generic, Data)

data Argument where
    Argument :: Text -> Value -> Argument
    deriving (Show, Eq, Generic, Data)

data ObjectField where
    ObjectField :: Text -> Value -> ObjectField
    deriving (Show, Eq, Generic, Data)

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
    deriving (Show, Eq, Generic, Data)

data Type where
    NamedType :: Text -> Type
    ListType :: Type -> Type
    NonNullNamedType :: Text -> Type
    NonNullListType :: Type -> Type
    deriving (Show, Eq, Generic, Data)
