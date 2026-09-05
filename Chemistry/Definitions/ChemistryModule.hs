module Chemistry.Definitions.ChemistryModule
    (
        CountedSpecies(..)
        ,CountedReaction(..)
        ,CountedResolvedReaction(..)
        ,ElementCounts(..)
    )where
import Chemistry.Definitions.Types
import Chemistry.Definitions.AST
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

data CountedSpecies =
    CountedSpecies
        { coefficientCounted :: Int
        , formulaCounted     :: Formula
        , elementCountsCounted :: ElementCounts
        }
    deriving (Eq, Show)

newtype CountedReaction =
    CountedReaction
        { 
            countedSpecies :: [CountedSpecies]
        }
    deriving (Eq, Show)

data CountedResolvedReaction =
    CountedResolvedReaction
        {
            countedReactantsResolved :: CountedReaction
            ,countedProductsResolved :: CountedReaction
        }
    deriving (Eq, Show)

newtype ElementCounts =
    ElementCounts (Map Element Integer)
    deriving (Eq,Show)