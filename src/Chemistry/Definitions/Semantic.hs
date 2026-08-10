module Chemistry.Definitions.Semantic
    (
        SemanticSpecies(..)
        ,ClassifiedReaction(..)
    )where
import Chemistry.Definitions.AST
    ( Formula )

import Chemistry.Definitions.Types
    ( Substance )

data SemanticSpecies = 
    SemanticSpecies
        {
            coefficientSemantic :: Int
            ,formulaSemantic :: Formula
            ,substanceSemantic :: Substance
        }
    deriving(Eq,Show)

newtype ClassifiedReaction = 
    ClassifiedReaction [SemanticSpecies]
    deriving(Eq,Show)