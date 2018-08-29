module GraphQL where
--

{-
Maybe tagless? Let's see
-}


class Resolver m where
    type ResRep m :: *


