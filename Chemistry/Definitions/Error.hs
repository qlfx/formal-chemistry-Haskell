module Chemistry.Definitions.Error
    (
        LexicalError(..),
        SyntaxError(..),
        SemanticError(..)
    )where

import Chemistry.Definitions.AST
    ( Formula )

import Chemistry.Definitions.Types
    ( Substance )

data LexicalError = 
    UnknownLexeme Int String
    deriving (Eq,Show)

data SyntaxError = 
    UnexpectedToken String
    deriving (Eq,Show)

-- data SemanticError
--     = InvalidCoefficient Int
--     | UnsupportedFormula String
--     | UnsupportedReactionCombination String
--     | UnsupportedReactantCount Int
--     deriving (Eq, Show)

data SemanticError
    = InvalidCoefficient Int
    | UnsupportedFormula Formula
    | UnsupportedReactionCombination [Substance]
    | UnsupportedReactantCount Int
    deriving (Eq, Show)