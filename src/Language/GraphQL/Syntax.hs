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

data OperationType = QUERY | MUTATION | SUBSCRIPTION
    deriving (Show, Eq, Generic, Data)

type SelectionSet = NonEmpty Selection

data Selection =
        Field (Maybe Text) Text (Maybe Arguments) (Maybe Directives) (Maybe SelectionSet)
    |   FragmentSpread Text (Maybe Directives)
    |   InlineFragment (Maybe Text) (Maybe Directives) SelectionSet
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

data Argument =
        Argument Text Value
    deriving (Show, Eq, Generic, Data)

type Directives = NonEmpty Directive

data Directive =
        Directive Text (Maybe Arguments)
    deriving (Show, Eq, Generic, Data)


data TypeSystemDefinitionNode =
        TSDNRoots RootOperationTypeDefinitionsNode
    |   TSDNDirective DirectiveDefinitionNode
    |   TSDNType TypeDefinitionNode
    deriving (Show, Eq, Generic, Data)

data TypeDefinitionNode =
        TDNScalar (Maybe Text) Text (Maybe Directives)
    |   TDNEnum (Maybe Text) Text (Maybe Directives) (Maybe (NonEmpty EnumValueDefNode))
    |   TDNObject (Maybe Text) Text (Maybe (NonEmpty Text)) (Maybe Directives) (Maybe (NonEmpty FieldDefinitionNode))
    |   TDNInterface (Maybe Text) Text (Maybe Directives) (Maybe (NonEmpty FieldDefinitionNode))
    |   TDNUnion (Maybe Text) Text (Maybe Directives) (Maybe (NonEmpty Text))
    |   TDNInput (Maybe Text) Text (Maybe Directives) (Maybe (NonEmpty InputValueDefinitionNode))
    |   TDN
    deriving (Show, Eq, Generic, Data)

data FieldDefinitionNode = FieldDefinition (Maybe Text) Text (Maybe ArgumentsDefinition) Type (Maybe Directives)
    deriving (Show, Eq, Generic, Data)

data EnumValueDefNode = EnumValueDef (Maybe Text) Text (Maybe Directives)
    deriving (Show, Eq, Generic, Data)

type RootOperationTypeDefinitionsNode = NonEmpty RootOperationTypeDefinitionNode

data RootOperationTypeDefinitionNode =
        ROTDNDefinition OperationType Text
    deriving (Show, Eq, Generic, Data)

data DirectiveDefinitionNode =
        DDNDefinition Text (Maybe ArgumentsDefinition) DirectiveLocation
    deriving (Show, Eq, Generic, Data)

type ArgumentsDefinition = NonEmpty InputValueDefinitionNode

data InputValueDefinitionNode =
        IVDN (Maybe Text) Text Type  (Maybe Value) (Maybe Directives)
    deriving (Show, Eq, Generic, Data)


data DirectiveLocation =
        DL_QUERY
    |   DL_MUTATION
    |   DL_SUBSCRIPTION
    |   DL_FIELD
    |   DL_FRAGMENT_DEFINITION
    |   DL_FRAGMENT_SPREAD
    |   DL_INLINE_FRAGMENT
    |   DL_SCHEMA
    |   DL_SCALAR
    |   DL_OBJECT
    |   DL_FIELD_DEFINITION
    |   DL_ARGUMENT_DEFINITION
    |   DL_INTERFACE
    |   DL_UNION
    |   DL_ENUM
    |   DL_ENUM_VALUE
    |   DL_INPUT_OBJECT
    |   DL_INPUT_FIELD_DEFINITION
    deriving (Show, Eq, Generic, Data)

data TypeSystemDefinitionExtensionNode = TSDEN -- NYI
    deriving (Show, Eq, Generic, Data)
