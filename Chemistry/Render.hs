module Chemistry.Render
    ( renderResult
    ) where

import Chemistry.Types
    ( Metal(..)
    , Acid(..)
    , Salt(..)
    , ReactionResult(..)
    )
import Chemistry.Parser (get_Metal_From_Salt)

metalSymbol :: Metal -> String
metalSymbol Zn = "Zn"
metalSymbol Fe = "Fe"
metalSymbol Cu = "Cu"

acidFormula :: Acid -> String
acidFormula HCl   = "HCl"
acidFormula H2SO4 = "H2SO4"

saltFormula :: Salt -> String
saltFormula ZnCl2 = "ZnCl2"
saltFormula FeCl2 = "FeCl2"
saltFormula ZnSO4 = "ZnSO4"
saltFormula FeSO4 = "FeSO4"
saltFormula CuCl2 = "CUCl2"
saltFormula CuSO4 = "CuSO4"

renderResult :: ReactionResult -> String
renderResult (NoReaction_Metal_Acid metal acid) =
    metalSymbol metal
        ++ " + "
        ++ acidFormula acid
        ++ " -> No reaction"

renderResult (ReactionOccurs_Metal_Acid metal HCl salt) =
    metalSymbol metal
        ++ " + 2HCl -> "
        ++ saltFormula salt
        ++ " + H2"

renderResult (ReactionOccurs_Metal_Acid metal H2SO4 salt) =
    metalSymbol metal
        ++ " + H2SO4 -> "
        ++ saltFormula salt
        ++ " + H2"

renderResult (NoReaction_Metal_Salt metal salt) = 
    metalSymbol metal ++ " + " ++ saltFormula salt ++ " -> No reaction"

renderResult (ReactionOccurs_Metal_Salt metal_reactant salt_reactant metal_product salt_product) = 
    metalSymbol metal_reactant ++ " + " ++ saltFormula salt_reactant ++ " -> " ++ metalSymbol metal_product ++ " + " ++ saltFormula salt_product 