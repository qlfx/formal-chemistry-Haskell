module Chemistry.Workers.ReactionBuilder
    (
        resolveReaction
        ,formulaFromMetal
        ,formulaFromAcid
        ,formulaFromSalt
        ,formulaFromElementalSubstance
    ) where

import Chemistry.Definitions.Types
import Chemistry.Definitions.AST
import Chemistry.Definitions.Resolved


resolveReaction :: ReactionResult -> Maybe ResolvedReaction

resolveReaction
    (ReactionOccurs_Metal_Acid
        metal
        acid
        salt
        elementalSubstance) =

    Just
        (ResolvedReaction
            {
                reactantsResolved =
                    [
                        speciesFromMetal metal
                        ,speciesFromAcid acid
                    ]

                ,productsResolved =
                    [
                        speciesFromSalt salt
                        ,speciesFromElementalSubstance
                            elementalSubstance
                    ]
            })


resolveReaction
    (ReactionOccurs_Metal_Salt
        metal
        reactantSalt
        elementalSubstance
        productSalt) =

    Just
        (ResolvedReaction
            {
                reactantsResolved =
                    [
                        speciesFromMetal metal
                        ,speciesFromSalt reactantSalt
                    ]

                ,productsResolved =
                    [
                        speciesFromElementalSubstance
                            elementalSubstance

                        ,speciesFromSalt productSalt
                    ]
            })


resolveReaction
    (NoReaction_Metal_Acid _ _) =
    Nothing


resolveReaction
    (NoReaction_Metal_Salt _ _) =
    Nothing


--------------------------------------------------
-- 生成 Species
--
-- 系数暂时设为 1。
-- 配平阶段将重新计算这些系数。
--------------------------------------------------

speciesFromMetal :: Metal -> Species

speciesFromMetal metal =
    Species
        {
            coefficientAST = 1
            ,formulaAST =
                formulaFromMetal metal
        }


speciesFromAcid :: Acid -> Species

speciesFromAcid acid =
    Species
        {
            coefficientAST = 1
            ,formulaAST =
                formulaFromAcid acid
        }


speciesFromSalt :: Salt -> Species

speciesFromSalt salt =
    Species
        {
            coefficientAST = 1
            ,formulaAST =
                formulaFromSalt salt
        }


speciesFromElementalSubstance :: ElementalSubstance -> Species

speciesFromElementalSubstance
    elementalSubstance =

    Species
        {
            coefficientAST = 1
            ,formulaAST =
                formulaFromElementalSubstance
                    elementalSubstance
        }


--------------------------------------------------
-- Metal -> Formula
--------------------------------------------------

formulaFromMetal :: Metal -> Formula

formulaFromMetal MetalZn =
    Formula
        [
            Atom Zn 1
        ]

formulaFromMetal MetalFe =
    Formula
        [
            Atom Fe 1
        ]

formulaFromMetal MetalCu =
    Formula
        [
            Atom Cu 1
        ]


--------------------------------------------------
-- Acid -> Formula
--------------------------------------------------

formulaFromAcid :: Acid -> Formula

formulaFromAcid AcidHCl =
    Formula
        [
            Atom H 1
            ,Atom Cl 1
        ]

formulaFromAcid AcidH2SO4 =
    Formula
        [
            Atom H 2
            ,Atom S 1
            ,Atom O 4
        ]


--------------------------------------------------
-- Salt -> Formula
--------------------------------------------------

formulaFromSalt :: Salt -> Formula

formulaFromSalt ZnCl2 =
    Formula
        [
            Atom Zn 1
            ,Atom Cl 2
        ]

formulaFromSalt FeCl2 =
    Formula
        [
            Atom Fe 1
            ,Atom Cl 2
        ]

formulaFromSalt CuCl2 =
    Formula
        [
            Atom Cu 1
            ,Atom Cl 2
        ]

formulaFromSalt ZnSO4 =
    Formula
        [
            Atom Zn 1
            ,Atom S 1
            ,Atom O 4
        ]

formulaFromSalt FeSO4 =
    Formula
        [
            Atom Fe 1
            ,Atom S 1
            ,Atom O 4
        ]

formulaFromSalt CuSO4 =
    Formula
        [
            Atom Cu 1
            ,Atom S 1
            ,Atom O 4
        ]


--------------------------------------------------
-- ElementalSubstance -> Formula
--------------------------------------------------

formulaFromElementalSubstance :: ElementalSubstance -> Formula

formulaFromElementalSubstance
    (ElementalMetal metal) =

    formulaFromMetal metal


formulaFromElementalSubstance
    ElementalHydrogen =

    Formula
        [
            Atom H 2
        ]