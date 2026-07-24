module Chemistry.Evaluator
    ( evaluate
    ) where


import Chemistry.Types
    ( Metal(..)
    , Acid(..)
    , Salt(..)
    , ReactionInput(..)
    , ReactionResult(..)
    )

import Chemistry.Parser
    (
        get_Metal_From_Salt
    )

activityRank :: Metal -> Int
activityRank Zn = 1
activityRank Fe = 2
activityRank Cu = 4

hydrogenRank :: Int
hydrogenRank = 3


canDisplaceHydrogen :: Metal -> Bool
canDisplaceHydrogen metal =
    activityRank metal < hydrogenRank

canDisplaceMetal :: Metal -> Salt -> Bool
canDisplaceMetal metal salt = 
    activityRank metal < activityRank (get_Metal_From_Salt salt)

--改到这里了

makeSalt :: Metal ->Either Acid Salt -> Maybe Salt
makeSalt Zn (Left HCl)   = Just ZnCl2
makeSalt Fe (Left HCl)   = Just FeCl2
makeSalt Zn (Left H2SO4) = Just ZnSO4
makeSalt Fe (Left H2SO4) = Just FeSO4
makeSalt Zn (Right FeCl2) = Just ZnCl2
makeSalt Zn (Right FeSO4) = Just ZnSO4
makeSalt Zn (Right CuCl2) = Just ZnCl2
makeSalt Zn (Right CuSO4) = Just ZnSO4
makeSalt Fe (Right CuCl2) = Just FeCl2
makeSalt Fe (Right CuSO4) = Just FeSO4
makeSalt _  _     = Nothing

evaluate :: ReactionInput -> ReactionResult
evaluate (ReactionInput_Metal_Acid metal acid)
    | not (canDisplaceHydrogen metal) =
        NoReaction_Metal_Acid metal acid

    | otherwise =
        case makeSalt metal (Left acid) of
            Just salt ->
                ReactionOccurs_Metal_Acid metal acid salt

            Nothing ->
                NoReaction_Metal_Acid metal acid
evaluate (ReactionInput_Metal_Salt metal salt)
    |not (canDisplaceMetal metal salt) = 
        NoReaction_Metal_Salt metal salt
    | otherwise = 
        case makeSalt metal (Right salt) of
            Just salt_reacted ->
                ReactionOccurs_Metal_Salt metal salt (get_Metal_From_Salt salt) salt_reacted

            Nothing ->
                NoReaction_Metal_Salt metal salt