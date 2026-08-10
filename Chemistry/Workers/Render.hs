module Chemistry.Workers.Render
    ( renderReactionResult
    ) where

import Chemistry.Definitions.Types

renderReactionResult :: ReactionResult -> String
renderReactionResult (ReactionOccurs_Metal_Acid metal acid salt elementalSubstance) = 
    renderReactionOccurs_Metal_Acid metal acid salt elementalSubstance

renderReactionResult (NoReaction_Metal_Acid metal acid) = 
    renderNoReaction_Metal_Acid metal acid

renderReactionResult (ReactionOccurs_Metal_Salt metal salt1 elementalSubstance salt2) = 
    renderReactionOccurs_Metal_Salt metal salt1 elementalSubstance salt2

renderReactionResult (NoReaction_Metal_Salt metal salt) = 
    renderNoReaction_Metal_Salt metal salt

renderReactionOccurs_Metal_Acid :: Metal -> Acid -> Salt -> ElementalSubstance -> String
renderReactionOccurs_Metal_Acid metal acid salt elementalSubstance = 
    renderMetal metal ++ "+" ++ renderAcid acid ++ "->" ++ renderSalt salt ++ "+" ++ renderElementalSubstance elementalSubstance

renderNoReaction_Metal_Acid :: Metal -> Acid -> String
renderNoReaction_Metal_Acid metal acid = 
    renderMetal metal ++ "+" ++ renderAcid acid ++ "->" ++ "No Reaction"

renderReactionOccurs_Metal_Salt :: Metal -> Salt -> ElementalSubstance -> Salt -> String
renderReactionOccurs_Metal_Salt metal salt1 elementalSubstance salt2 = 
    renderMetal metal ++ "+" ++ renderSalt salt1 ++ "->" ++ renderElementalSubstance elementalSubstance ++ "+" ++ renderSalt salt2

renderNoReaction_Metal_Salt :: Metal -> Salt -> String
renderNoReaction_Metal_Salt metal salt = 
    renderMetal metal ++"+" ++ renderSalt salt ++ "->" ++ "No Reaction"

renderMetal :: Metal -> String
renderMetal MetalZn = "Zn"
renderMetal MetalFe = "Fe"
renderMetal MetalCu = "Cu"

renderAcid :: Acid -> String
renderAcid AcidHCl = "HCl"
renderAcid AcidH2SO4 = "H2SO4"

renderSalt :: Salt -> String
renderSalt ZnCl2 = "ZnCl2"
renderSalt FeCl2 = "FeCl2"
renderSalt CuCl2 = "CuCl2"
renderSalt ZnSO4 = "ZnSO4"
renderSalt FeSO4 = "FeSO4"
renderSalt CuSO4 = "CuSO4"

renderElementalSubstance :: ElementalSubstance -> String
renderElementalSubstance (ElementalMetal metal) = renderMetal metal
renderElementalSubstance ElementalHydrogen = "H2"