module Driver where
--
-- import Data.Attoparsec.Text
-- import Data.List.NonEmpty (NonEmpty ((:|)))
-- import qualified Data.HashMap.Strict as M
-- import Data.Text
-- import Language.GraphQL.Parse
-- import Language.GraphQL.Syntax
-- import GraphQL.Resolve


-- r1 :: Resolvers
-- r1 = M.fromList [
--     ("amount", Resolver "$100.0032"),
--     ("currency", Resolver "USD"),
--     ("birthday", Resolver "happy")
--     ]

-- q1 :: Text
-- q1 = "{ amount currency}"

-- qd1 :: Either [Char] Document
-- qd1 = parseOnly document q1

-- qx1 :: OpDef
-- qx1 =
--     let def = unpackDef $ either (error "nyi") id qd1
--      in unpackSels def
--     where
--         unpackSels (ExecutableDefinition (OpDefExecutableDefinition x)) = x
--         unpackSels _ = error "NYI"
--         unpackDef (Document (x :| _)) = x

-- go :: [Text]
-- go = maybe (error "failed to run") id $ runResolvers r1 qx1
