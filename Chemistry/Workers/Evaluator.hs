module Chemistry.Workers.Evaluator
    ( evaluateReaction
    , reactionOccurs
    , comparisonOf
    , moreActiveThan
    , activityRank
    , evaluateMetalAcid
    , evaluateMetalSalt
    , saltFromMetalAcid
    , replaceMetalInSalt
    ) where
import Chemistry.Definitions.Types
import Chemistry.Workers.Analyzer

evaluateReaction :: ReactionInput -> ReactionResult
evaluateReaction reactionInput@(ReactionInput_Metal_Acid metal acid) = evaluateMetalAcid (reactionOccurs reactionInput) metal acid
evaluateReaction reactionInput@(ReactionInput_Metal_Salt metal salt) = evaluateMetalSalt (reactionOccurs reactionInput) metal salt

evaluateMetalAcid :: Bool -> Metal -> Acid -> ReactionResult
evaluateMetalAcid canReactionOccur metal acid =
    case canReactionOccur of
        True ->
            ReactionOccurs_Metal_Acid
                {
                    metalAcidReactantMetal = metal
                    ,metalAcidReactantAcid = acid
                    ,metalAcidProductSalt = saltFromMetalAcid metal acid
                    ,metalAcidProductElementalSubstance = ElementalHydrogen
                }
        False ->
            NoReaction_Metal_Acid
                {
                    noReactionMetalAcidMetal = metal
                    ,noReactionMetalAcidAcid = acid
                }

evaluateMetalSalt :: Bool -> Metal -> Salt -> ReactionResult
evaluateMetalSalt canReactionOccur metal salt =
    case canReactionOccur of
        True ->
            ReactionOccurs_Metal_Salt
                {
                    metalSaltReactantMetal = metal
                    ,metalSaltReactantSalt = salt
                    ,metalSaltProductElementalSubstance =
                        ElementalMetal (metalOfSalt salt)
                    ,metalSaltProductSalt =
                        replaceMetalInSalt metal salt
                }
        False ->
            NoReaction_Metal_Salt
                {
                    noReactionMetalSaltMetal = metal
                    ,noReactionMetalSaltSalt = salt
                }

comparisonOf :: ReactionInput -> (ElementalSubstance,ElementalSubstance)
comparisonOf (ReactionInput_Metal_Acid metal _) =
    (
        ElementalMetal metal
        ,ElementalHydrogen
    )
comparisonOf
    (ReactionInput_Metal_Salt metal salt) =
    (
        ElementalMetal metal
        ,ElementalMetal (metalOfSalt salt)
    )

reactionOccurs :: ReactionInput -> Bool
reactionOccurs reactionInput =
    case comparisonOf reactionInput of
        (firstSubstance, secondSubstance) ->
            moreActiveThan
                firstSubstance
                secondSubstance

moreActiveThan :: ElementalSubstance -> ElementalSubstance -> Bool
moreActiveThan firstSubstance secondSubstance =
    activityRank firstSubstance > activityRank secondSubstance

activityRank :: ElementalSubstance -> Int
activityRank (ElementalMetal MetalZn) = 4
activityRank (ElementalMetal MetalFe) = 3
activityRank ElementalHydrogen = 2
activityRank (ElementalMetal MetalCu) = 1

saltFromMetalAcid :: Metal -> Acid -> Salt
saltFromMetalAcid MetalZn AcidHCl = ZnCl2
saltFromMetalAcid MetalFe AcidHCl = FeCl2
saltFromMetalAcid MetalCu AcidHCl = CuCl2
saltFromMetalAcid MetalZn AcidH2SO4 = ZnSO4
saltFromMetalAcid MetalFe AcidH2SO4 = FeSO4
saltFromMetalAcid MetalCu AcidH2SO4 = CuSO4

replaceMetalInSalt :: Metal -> Salt -> Salt
--------------------------------------------------
-- 氯化物
--------------------------------------------------
replaceMetalInSalt MetalZn ZnCl2 = ZnCl2
replaceMetalInSalt MetalZn FeCl2 = ZnCl2
replaceMetalInSalt MetalZn CuCl2 = ZnCl2
replaceMetalInSalt MetalFe ZnCl2 = FeCl2
replaceMetalInSalt MetalFe FeCl2 = FeCl2
replaceMetalInSalt MetalFe CuCl2 = FeCl2
replaceMetalInSalt MetalCu ZnCl2 = CuCl2
replaceMetalInSalt MetalCu FeCl2 = CuCl2
replaceMetalInSalt MetalCu CuCl2 = CuCl2
--------------------------------------------------
-- 硫酸盐
--------------------------------------------------
replaceMetalInSalt MetalZn ZnSO4 = ZnSO4
replaceMetalInSalt MetalZn FeSO4 = ZnSO4
replaceMetalInSalt MetalZn CuSO4 = ZnSO4
replaceMetalInSalt MetalFe ZnSO4 = FeSO4
replaceMetalInSalt MetalFe FeSO4 = FeSO4
replaceMetalInSalt MetalFe CuSO4 = FeSO4
replaceMetalInSalt MetalCu ZnSO4 = CuSO4
replaceMetalInSalt MetalCu FeSO4 = CuSO4
replaceMetalInSalt MetalCu CuSO4 = CuSO4