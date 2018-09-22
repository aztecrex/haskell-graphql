module GraphQL.Resolve where
--
import Data.Text (Text)
import Language.GraphQL.Syntax

import qualified Data.HashMap.Strict as M

type Resolvers = M.HashMap Text Resolver

data Resolver where
    Resolver :: Text -> Resolver

runResolvers :: Resolvers -> OpDef -> Maybe [Text]
runResolvers ress op =
    let names = fmap selName  (sels op)
        maybeResolve = mapM (flip M.lookup ress) names
    in maybeResolve >>= mapM (\(Resolver v) -> pure v)
    where
        sels (SelSetOperationDefinition (SelectionSet x)) = x
        sels (OpTypeOperationDefinition _ _ _ _ (SelectionSet x)) = x
        selName (Field _ x _ _ _) = x
        selName _ = error "NYI"

