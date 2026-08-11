module Chemistry.Definitions.Types
    ( Metal(..)
    ,Element(..)
    , Acid(..)
    , Salt(..)
    , ReactionInput(..)
    , ReactionResult(..)
    ,Substance(..)
    ,metalOfSalt
    ,ElementalSubstance(..)
    ) where

data Element = 
    H | O | S | Cl | Zn | Fe | Cu
    deriving(Eq,Show) 

data Metal
    = MetalZn
    | MetalFe
    | MetalCu
    deriving (Eq, Show)

data Acid
    = AcidHCl
    | AcidH2SO4
    deriving (Eq, Show)

data Salt
    = ZnCl2
    | FeCl2
    | ZnSO4
    | FeSO4
    | CuCl2
    | CuSO4
    deriving (Eq, Show)

data ElementalSubstance
    = ElementalMetal Metal
    | ElementalHydrogen
    deriving (Eq, Show)
    
data ReactionInput
    = ReactionInput_Metal_Acid Metal Acid
    |ReactionInput_Metal_Salt Metal Salt
    deriving (Eq, Show)

data ReactionResult
    = ReactionOccurs_Metal_Acid
        { metalAcidReactantMetal :: Metal
        , metalAcidReactantAcid :: Acid
        , metalAcidProductSalt :: Salt
        , metalAcidProductElementalSubstance :: ElementalSubstance
        }

    | NoReaction_Metal_Acid
        { noReactionMetalAcidMetal :: Metal
        , noReactionMetalAcidAcid :: Acid
        }

    | ReactionOccurs_Metal_Salt
        { metalSaltReactantMetal :: Metal
        , metalSaltReactantSalt :: Salt
        , metalSaltProductElementalSubstance :: ElementalSubstance
        , metalSaltProductSalt :: Salt
        }

    | NoReaction_Metal_Salt
        { noReactionMetalSaltMetal :: Metal
        , noReactionMetalSaltSalt :: Salt
        }
    deriving (Eq, Show)

data Substance = 
    SubstanceMetal Metal | SubstanceAcid Acid | SubstanceSalt Salt
    deriving (Eq,Show)

metalOfSalt :: Salt -> Metal
metalOfSalt ZnCl2 = MetalZn
metalOfSalt ZnSO4 = MetalZn
metalOfSalt FeCl2 = MetalFe
metalOfSalt FeSO4 = MetalFe
metalOfSalt CuCl2 = MetalCu
metalOfSalt CuSO4 = MetalCu