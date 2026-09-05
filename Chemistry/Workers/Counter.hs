-- We can consider the Set of SpeciesCounted as a norm in algebra
module Chemistry.Workers.Counter
    (countSpeciesList
    , countFormula
    , countOf
    , countsToList

    ,countResolvedReaction
    ,countReactionAST
    ) where

import Chemistry.Definitions.Types

import Chemistry.Definitions.AST
    ( Formula(..)
    , FormulaPart(..)
    , Species(..)
    , ReactionAST(..)
    )
import Chemistry.Definitions.ChemistryModule
import Data.Map.Strict ( Map )

import qualified Data.Map.Strict as Map
import Chemistry.Definitions.Resolved

--------------------------------------------------
-- 元素计数
--------------------------------------------------

--------------------------------------------------
-- 计数后的物质和反应
--------------------------------------------------



--------------------------------------------------
-- ReactionAST
--------------------------------------------------

countSpeciesList :: [Species] -> CountedReaction
countSpeciesList speciesList = 
    CountedReaction
        {
            countedSpecies = 
                map countOneSpecies speciesList
        }
--------------------------------------------------
-- Species
--------------------------------------------------

countOneSpecies :: Species -> CountedSpecies
countOneSpecies
    (Species coefficient formula) =

    CountedSpecies
        { coefficientCounted = coefficient
        , formulaCounted = formula
        , elementCountsCounted =
            countFormula formula
        }


--------------------------------------------------
-- Formula
--------------------------------------------------

countFormula :: Formula -> ElementCounts
countFormula (Formula formulaParts) =
    foldr
        elementalAddition
        emptyElementCounts
        (map countFormulaPart formulaParts)


--------------------------------------------------
-- FormulaPart
--------------------------------------------------

countFormulaPart :: FormulaPart -> ElementCounts
countFormulaPart formulaPart =
    case formulaPart of

        Atom element multiplicity ->
            countAtom element multiplicity

        Group innerFormula multiplicity ->
            countGroup innerFormula multiplicity


--------------------------------------------------
-- Atom
--------------------------------------------------

countAtom :: Element -> Int -> ElementCounts
countAtom element multiplicity =
    ElementCounts
        (Map.singleton
            element
            (toInteger multiplicity))


--------------------------------------------------
-- Group
--------------------------------------------------

countGroup :: Formula -> Int -> ElementCounts
countGroup innerFormula multiplicity =
    elementalMultiple
        (toInteger multiplicity)
        (countFormula innerFormula)

--------------------------------------------------
-- 元素计数加法
--------------------------------------------------

emptyElementCounts :: ElementCounts
emptyElementCounts = ElementCounts Map.empty

elementalAddition :: ElementCounts -> ElementCounts -> ElementCounts
elementalAddition (ElementCounts left) (ElementCounts right) =
    ElementCounts (Map.unionWith (+) left right)


--------------------------------------------------
-- 元素计数数乘
--------------------------------------------------

elementalMultiple :: Integer -> ElementCounts -> ElementCounts
elementalMultiple multiplier (ElementCounts counts) =
    ElementCounts
        (Map.map
            (multiplier *)
            counts)

--------------------------------------------------
-- 查询接口
--------------------------------------------------

countOf :: Element -> ElementCounts -> Integer
countOf element (ElementCounts counts) = Map.findWithDefault 0 element counts

countsToList :: ElementCounts -> [(Element, Integer)]
countsToList (ElementCounts counts) = Map.toAscList counts



countReactionAST :: ReactionAST -> CountedReaction
countReactionAST (ReactionAST speciesList) = countSpeciesList speciesList

countResolvedReaction :: ResolvedReaction -> CountedResolvedReaction
countResolvedReaction resolvedReaction = 
    CountedResolvedReaction         
        {
            countedReactantsResolved = countSpeciesList (reactantsResolved resolvedReaction)
            ,
            countedProductsResolved = countSpeciesList (productsResolved resolvedReaction)
        }