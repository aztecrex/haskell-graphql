module GraphQLSpike where
--
import Prelude hiding (empty, putStrLn)
import Data.GraphQL.AST
import Data.GraphQL.Encoder
import Data.GraphQL.Parser

import Control.Applicative
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text hiding (empty)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (putStrLn)

import Data.Time

import Debug.Trace

-- schema1 :: Alternative f => Schema f
-- schema1 = hello :| []

-- hello :: Alternative f => Resolver f
-- hello = Schema.scalar "hello" ("it's me" :: Text)

-- these are from tutorial but they don't compile ^^^^^^

-- moving on to creating a DSL. we'll see
