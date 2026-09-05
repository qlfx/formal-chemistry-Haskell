module Chemistry.Workers.IRBuilder
    (
        buildBalanceIR
    ) where

import Chemistry.Definitions.Types
    (
        Element
    )
import Chemistry.Definitions.Resolved
    (
        ResolvedReaction
    )
import Chemistry.Definitions.ChemistryModule
import Chemistry.Definitions.BalanceIR
import Chemistry.Workers.Counter
    (   countResolvedReaction
        ,countOf
        ,countsToList
    )
import qualified Data.Set as Set

--------------------------------------------------
-- 总入口
--------------------------------------------------

buildBalanceIR :: ResolvedReaction -> BalanceIR
buildBalanceIR resolvedReaction =
    let
        countedResolvedReaction = countResolvedReaction resolvedReaction

        elementList = collectElementBasis countedResolvedReaction

        speciesVariables = buildSpeciesVariable countedResolvedReaction

        matrix = buildStoichiometricMatrix elementList countedResolvedReaction
    in
        BalanceIR
            {
                elementBasisIR = elementList
                ,speciesBasisIR = speciesVariables
                ,stoichiometricMatrixIR =  matrix
            }

--------------------------------------------------
-- 收集元素基
--------------------------------------------------

collectElementBasis :: CountedResolvedReaction -> [Element]

collectElementBasis countedResolvedReaction =
    let
        reactants =
            countedSpecies
                (countedReactantsResolved
                    countedResolvedReaction)

        products =
            countedSpecies
                (countedProductsResolved
                    countedResolvedReaction)

        allSpecies =
            reactants ++ products

        allElements =
            concatMap
                elementsFromCountedSpecies
                allSpecies

    in
        Set.toAscList
            (Set.fromList allElements)


elementsFromCountedSpecies :: CountedSpecies -> [Element]

elementsFromCountedSpecies species =
    map fst (countsToList (elementCountsCounted species))


--------------------------------------------------
-- 建立物种基
--------------------------------------------------
buildSpeciesVariable :: CountedResolvedReaction -> [SpeciesVariable]
buildSpeciesVariable countedResolvedReaction =
    let
        reactants =
            countedSpecies
                (countedReactantsResolved
                    countedResolvedReaction)

        products =
            countedSpecies
                (countedProductsResolved
                    countedResolvedReaction)

        reactantVariables =
            zipWith
                (makeSpeciesVariable
                    ReactantSide)
                [1 ..]
                reactants

        firstProductIndex = length reactants + 1
        productVariables =
            zipWith
                (makeSpeciesVariable
                    ProductSide)
                [firstProductIndex ..]
                products
    in
        reactantVariables
            ++ productVariables


makeSpeciesVariable :: ReactionSide -> Int -> CountedSpecies -> SpeciesVariable
makeSpeciesVariable side variableIndex species =
    SpeciesVariable
        {
            variableIndexIR = variableIndex
            ,sideIR = side
            ,formulaIR = formulaCounted species
        }

--------------------------------------------------
-- 建立计量矩阵
--------------------------------------------------

buildStoichiometricMatrix :: [Element] -> CountedResolvedReaction -> [[Integer]]
buildStoichiometricMatrix elementList countedResolvedReaction =
    map
        (\element ->
            buildElementRow element countedResolvedReaction)
        elementList


--------------------------------------------------
-- 建立一个元素对应的矩阵行
--------------------------------------------------
buildElementRow :: Element -> CountedResolvedReaction -> [Integer]
buildElementRow element countedResolvedReaction =
    let
        reactants =
            countedSpecies (countedReactantsResolved countedResolvedReaction)

        products =
            countedSpecies (countedProductsResolved countedResolvedReaction)

        reactantValues =
            map (signedElementCount ReactantSide element) reactants

        productValues =
            map (signedElementCount ProductSide element) products

    in
        reactantValues
            ++ productValues

--------------------------------------------------
-- 有符号元素计数
--------------------------------------------------
signedElementCount :: ReactionSide -> Element -> CountedSpecies -> Integer
signedElementCount ReactantSide element species =
    negate (countOf element (elementCountsCounted species))

signedElementCount ProductSide element species =
    countOf element (elementCountsCounted species)