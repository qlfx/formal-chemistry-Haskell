module Chemistry.Token
    (
        Token(..)
    ) where

data Token
    = TokZn
    | TokFe
    | TokCu
    | TokHCl
    | TokH2SO4
    | TokPlus
    | TokEOF
    deriving (Eq, Show)