module Chemistry.Definitions.BalanceIR
    (
        ReactionSide(..)
        ,SpeciesVariable(..)
        ,BalanceIR(..)
    )where
import Chemistry.Definitions.Types
import Chemistry.Definitions.AST

data ReactionSide = 
    ReactantSide | ProductSide
    deriving(Eq,Show)

data SpeciesVariable = 
    SpeciesVariable
        {
            variableIndexIR :: Int
            ,sideIR :: ReactionSide
            ,formulaIR :: Formula
        }
    deriving(Eq,Show)

data BalanceIR = 
    BalanceIR
        {
            elementBasisIR :: [Element]
            ,speciesBasisIR :: [SpeciesVariable]
            ,stoichiometricMatrixIR :: [[Integer]]
        }
    deriving(Eq,Show)