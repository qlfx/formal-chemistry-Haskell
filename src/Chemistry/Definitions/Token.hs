module Chemistry.Definitions.Token
    (
        Token(..)
    ) where
import Chemistry.Definitions.Types

data Token
    =
        TokElement Element
        | TokNumber Int
        | TokPlus
        | TokLParen
        | TokRParen
        | TokEOF
    deriving (Eq, Show)