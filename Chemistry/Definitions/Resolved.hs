module Chemistry.Definitions.Resolved
    (
        ResolvedReaction(..)
    ) where

import Chemistry.Definitions.AST
    (
        Species
    )

data ResolvedReaction =
    ResolvedReaction
        {
            reactantsResolved :: [Species]
            ,productsResolved :: [Species]
        }
    deriving (Eq, Show)