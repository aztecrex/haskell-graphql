{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes  #-}
module Language.GraphQL.TH (graphql) where
--
import Data.Attoparsec.Text (parseOnly)
import Data.Data (Data)
import Data.Text (pack, unpack, Text)
import Data.Typeable (cast)
import Language.GraphQL.Parse (document)
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ)
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH (Exp(AppE, VarE), Q)

graphql :: QuasiQuoter
graphql = QuasiQuoter {
    quoteExp  = parsed,
    quotePat  = undefined,
    quoteType = undefined,
    quoteDec  = undefined
    }

parsed :: String -> Q Exp
parsed ssrc = do
    let src = pack ssrc
    case parseOnly document src of
        Left err -> fail err
        Right doc -> liftWithText doc

liftWithText :: Data a => a -> Q Exp
liftWithText = dataToExpQ(\a -> liftText <$> cast a)

liftText :: Text -> Q Exp
liftText v = AppE (VarE 'pack) <$> lift (unpack v)

