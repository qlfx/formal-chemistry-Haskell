module Chemistry.AST
    (
        Element(..),Formula(..),FormulaPart(..),Species(..),ReactionAST(..)
    )where

data Element = 
    H | O | S | Cl | Zn | Fe | Cu
    deriving(Eq,Show) 

data Formula = 
    Formula [FormulaPart]
    deriving(Eq,Show)

data FormulaPart = 
    Atom Element Int   --H2
    | Group Formula Int --(OH)2
    deriving(Eq,Show)

data Species = Species
    {
        coefficientAST :: Int,
        formulaAST :: Formula
    }
    deriving(Eq,Show)

newtype ReactionAST = 
    ReactionAST
        {
            reactantAST :: [Species]
        }
    deriving(Eq,Show)