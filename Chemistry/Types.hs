module Chemistry.Types
    ( Metal(..)
    , Acid(..)
    , Salt(..)
    , ReactionInput(..)
    , ReactionResult(..)
    ,metalOfSalt
    ) where

data Metal
    = Zn
    | Fe
    | Cu
    deriving (Eq, Show)

data Acid
    = HCl
    | H2SO4
    deriving (Eq, Show)

data Salt
    = ZnCl2
    | FeCl2
    | ZnSO4
    | FeSO4
    | CuCl2
    | CuSO4
    deriving (Eq, Show)

data ReactionInput
    = ReactionInput_Metal_Acid Metal Acid
    |ReactionInput_Metal_Salt Metal Salt
    deriving (Eq, Show)

data ReactionResult
    = ReactionOccurs_Metal_Acid Metal Acid Salt
    | NoReaction_Metal_Acid Metal Acid
    |ReactionOccurs_Metal_Salt Metal Salt Metal Salt
    |NoReaction_Metal_Salt Metal Salt
    deriving (Eq, Show)

metalOfSalt :: Salt -> Metal
metalOfSalt ZnCl2 = Zn
metalOfSalt ZnSO4 = Zn
metalOfSalt FeCl2 = Fe
metalOfSalt FeSO4 = Fe
metalOfSalt CuCl2 = Cu
metalOfSalt CuSO4 = Cu