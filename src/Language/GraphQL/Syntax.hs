{-# LANGUAGE DeriveDataTypeable #-}
module Language.GraphQL.Syntax where

--
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics

type DocumentNode = NonEmpty DefinitionNode

data DefinitionNode =
        DNExecutable ExecutableDefinitionNode
    |   DNTypeSystem TypeSystemDefinitionNode
    |   DNTypeSystemExtension TypeSystemDefinitionExtensionNode
        deriving (Show, Eq, Generic, Data)

data ExecutableDefinitionNode =
        EDNOperation OperationDefinitionNode
    |   EDNFragment FragmentDefinitionNode
        deriving (Show, Eq, Generic, Data)

data OperationDefinitionNode =
        ODNSelectionSet SelectionSet
    |   ODNTyped OperationType (Maybe Text) (Maybe VariableDefinitions) (Maybe Directives) SelectionSet
    deriving (Show, Eq, Generic, Data)

data OperationType = QUERY
    -- MUTATION :: OperationType
    -- SUBSCRIPTION :: OperationType
    deriving (Show, Eq, Generic, Data)

type SelectionSet = [Selection]

data Selection =
        Field (Maybe Text) Text (Maybe Arguments) (Maybe Directives) (Maybe SelectionSet)
    -- |   FragmentSpread Text (Maybe Directives)
    -- |   InlineFragment (Maybe TypeCondition) (Maybe Directives) SelectionSet
    deriving (Show, Eq, Generic, Data)

type VariableDefinitions = NonEmpty VariableDefinition

data VariableDefinition =
        VariableDefinition Text Type (Maybe Value)
    deriving (Show, Eq, Generic, Data)

data Type =
        TNamed Text Bool
    |   TList Type Bool
    deriving (Show, Eq, Generic, Data)

data Value =
        VVariable Text
    |   VInt Int  -- 32 bits precision per GQL spec
    |   VFloat Double -- 64 bits precision per GQL spec
    |   VString Text
    |   VBool Bool
    |   VNull
    |   VEnum Text
    |   VList [Value]
    |   VObject[(Text, Value)]
    deriving (Show, Eq, Generic, Data)

data FragmentDefinitionNode =
        FragmentDefinition Text Text (Maybe Directives) SelectionSet
    deriving (Show, Eq, Generic, Data)

type Arguments = NonEmpty Argument

-- data Argument where
--     Argument :: Text -> Value -> Argument
--     deriving (Show, Eq, Generic, Data)


data Argument =
        Argument Text Value
    deriving (Show, Eq, Generic, Data)
data Directives = DS -- NYI
    deriving (Show, Eq, Generic, Data)


data TypeSystemDefinitionNode = TSDN -- NYI
    deriving (Show, Eq, Generic, Data)
data TypeSystemDefinitionExtensionNode = TSDEN -- NYI
    deriving (Show, Eq, Generic, Data)



-- data ExecDef where
--     OpDefExecutableDefinition :: OpDef -> ExecDef
--     FragmentDefinition :: FragmentName -> TypeCondition -> Maybe Directives -> ExecDef
--     deriving (Show, Eq, Generic, Data)

-- data TypeSysDef where
--     SchemaDefinition :: Maybe Directives -> NonEmpty RootOperationTypeDefinition -> TypeSysDef
--     TypeDefinition :: TypeSysDef
--     DirectiveDefinition :: TypeSysDef
--     deriving (Show, Eq, Generic, Data)

-- data TypeDef where
--     ScalarTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> TypeDef
--     ObjectTypeDefinition :: Text -> Maybe ImplemntsIfcs -> Maybe Directives -> Maybe FieldsDef -> TypeDef
--     InterfaceTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe FieldsDef -> TypeDef
--     UnionTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe UnionMemTypes -> TypeDef
--     EnumTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe EnumValsDef -> TypeDef
--     InputObjectTypeDefinition :: Maybe Text -> Text -> Maybe Directives -> Maybe InputFieldsDefinition -> TypeDef
--     deriving (Show, Eq, Generic, Data)

-- data InputFieldsDefinition where
--     InputFieldsDefinition :: NonEmpty InputValueDefinition  -> InputFieldsDefinition
--     deriving (Show, Eq, Generic, Data)

-- data EnumValsDef where
--     EnumValuesDefinition :: NonEmpty EnumValDef -> EnumValsDef
--     deriving (Show, Eq, Generic, Data)

-- data EnumValDef where
--     EnumValueDef :: Maybe Text -> Text -> Maybe Directives -> EnumValDef
--     deriving (Show, Eq, Generic, Data)



-- data ImplemntsIfcs where
--     ImplementsInterfaces :: NonEmpty Text -> ImplemntsIfcs
--     deriving (Show, Eq, Generic, Data)

-- data FieldsDef where
--     FieldsDefinition :: NonEmpty FieldDef -> FieldsDef
--     deriving (Show, Eq, Generic, Data)

-- data FieldDef where
--     FieldDefinition :: Maybe Text -> Text -> Maybe ArgsDef -> Type -> Maybe Directives -> FieldDef
--     deriving (Show, Eq, Generic, Data)

-- data UnionMemTypes where
--     UnionMemberTypes :: NonEmpty Text -> UnionMemTypes
--     deriving (Show, Eq, Generic, Data)

-- data ArgsDef where
--     ArgumentsDefinition :: NonEmpty InputValueDefinition -> ArgsDef
--     deriving (Show, Eq, Generic, Data)

-- data InputValueDefinition where
--     InputValueDefinition :: Maybe Text -> Text -> Type -> Maybe DefaultValue -> Maybe Directives -> InputValueDefinition
--     deriving (Show, Eq, Generic, Data)



-- data RootOperationTypeDefinition where
--     RootOperationTypeDefinition :: Text -> OperationType -> RootOperationTypeDefinition
--     deriving (Show, Eq, Generic, Data)


-- data TypeSysExt where
--     SchemaExtension :: Maybe Directives -> [OperationTypeDefinition] -> TypeSysExt
--     TypeExtension :: TypeExt -> TypeSysExt
--     deriving (Show, Eq, Generic, Data)

-- data OperationTypeDefinition where
--     OperationTypeDefinition :: OperationType -> Text -> OperationTypeDefinition
--     deriving (Show, Eq, Generic, Data)

-- data TypeExt where
--     ScalarTypeExtension :: Text -> Maybe Directives -> TypeExt
--     ObjectTypeExtension :: Text -> Maybe ImplemntsIfcs -> Maybe Directives -> Maybe FieldsDef -> TypeExt
--     InterfaceTypeExtension :: Text -> Maybe Directives -> Maybe FieldsDef -> TypeExt
--     UnionTypeExtension :: Text -> Maybe Directives -> Maybe UnionMemTypes -> TypeExt
--     EnumTypeExtension :: Text -> Maybe Directives -> EnumValsDef -> TypeExt
--     InputObjectTypeExtension :: Text -> Maybe Directives -> InputFieldsDefinition -> TypeExt
--     deriving (Show, Eq, Generic, Data)

-- data OperationType where
--     QUERY :: OperationType
--     MUTATION :: OperationType
--     SUBSCRIPTION :: OperationType
--     deriving (Show, Eq, Generic, Data)

-- data VariableDefinitions where
--     VariableDefinitions :: [VariableDefinition] -> VariableDefinitions
--     deriving (Show, Eq, Generic, Data)

-- data VariableDefinition where
--     VariableDefinition :: Type -> Maybe DefaultValue -> VariableDefinition
--     deriving (Show, Eq, Generic, Data)

-- data Variable where
--     Variable :: Text -> Variable
--     deriving (Show, Eq, Generic, Data)

-- data DefaultValue where
--     DefaultValue :: Text -> DefaultValue
--     deriving (Show, Eq, Generic, Data)

-- data Directives where
--     Directives :: NonEmpty Directive -> Directives
--     deriving (Show, Eq, Generic, Data)

-- data Directive where
--     Directive :: Text -> Arguments -> Directive
--     deriving (Show, Eq, Generic, Data)

-- data FragmentName where
--     FragmentName :: Text -> FragmentName
--     deriving (Show, Eq, Generic, Data)

-- data TypeCondition where
--     TypeCondition :: Text -> TypeCondition
--     deriving (Show, Eq, Generic, Data)

-- data Alias where
--     Alias :: Text -> Alias
--     deriving (Show, Eq, Generic, Data)

-- data Arguments where
--     Arguments :: [Argument] -> Arguments
--     deriving (Show, Eq, Generic, Data)

-- data Argument where
--     Argument :: Text -> Value -> Argument
--     deriving (Show, Eq, Generic, Data)

-- data ObjectField where
--     ObjectField :: Text -> Value -> ObjectField
--     deriving (Show, Eq, Generic, Data)

-- data Value where
--     VariableValue :: Variable -> Value
--     IntValue :: Int32 -> Value
--     FloatValue :: Double -> Value
--     StringValue :: Text -> Value
--     BooleanValue :: Bool -> Value
--     NullValue :: Value
--     EnumValue :: Text -> Value
--     ListValue :: [Value] -> Value
--     ObjectValue :: [ObjectField] -> Value
--     deriving (Show, Eq, Generic, Data)

-- data Type where
--     NamedType :: Text -> Type
--     ListType :: Type -> Type
--     NonNullNamedType :: Text -> Type
--     NonNullListType :: Type -> Type
--     deriving (Show, Eq, Generic, Data)

