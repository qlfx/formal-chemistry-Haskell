module Main where

import Chemistry.Definitions.Types
    (
        Element(..)
    )

import Chemistry.Definitions.AST
    (
        Formula(..)
        ,FormulaPart(..)
        ,Species(..)
    )

import Chemistry.Definitions.Resolved
    (
        ResolvedReaction(..)
    )

import Chemistry.Definitions.BalanceIR
    (
        ReactionSide(..)
        ,SpeciesVariable(..)
        ,BalanceIR(..)
    )

import Chemistry.Workers.IRBuilder
    (
        buildBalanceIR
    )


--------------------------------------------------
-- 简单相等测试
--------------------------------------------------

assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> IO ()

assertEqual
    testName
    expected
    actual =

    if expected == actual
        then
            putStrLn
                ("[PASS] " ++ testName)

        else do
            putStrLn
                ("[FAIL] " ++ testName)

            putStrLn
                ("  expected: " ++ show expected)

            putStrLn
                ("  actual:   " ++ show actual)


--------------------------------------------------
-- 测试使用的 Formula
--------------------------------------------------

formulaZn :: Formula

formulaZn =
    Formula
        [
            Atom Zn 1
        ]


formulaHCl :: Formula

formulaHCl =
    Formula
        [
            Atom H 1
            ,Atom Cl 1
        ]


formulaZnCl2 :: Formula

formulaZnCl2 =
    Formula
        [
            Atom Zn 1
            ,Atom Cl 2
        ]


formulaH2 :: Formula

formulaH2 =
    Formula
        [
            Atom H 2
        ]


--------------------------------------------------
-- 已经确定产物的完整反应
--
-- Zn + HCl -> ZnCl2 + H2
--------------------------------------------------

testReaction :: ResolvedReaction

testReaction =
    ResolvedReaction
        {
            reactantsResolved =
                [
                    Species
                        {
                            coefficientAST = 1
                            ,formulaAST = formulaZn
                        }

                    ,Species
                        {
                            coefficientAST = 1
                            ,formulaAST = formulaHCl
                        }
                ]

            ,productsResolved =
                [
                    Species
                        {
                            coefficientAST = 1
                            ,formulaAST = formulaZnCl2
                        }

                    ,Species
                        {
                            coefficientAST = 1
                            ,formulaAST = formulaH2
                        }
                ]
        }


--------------------------------------------------
-- 预期元素基
--
-- Set.toAscList 按 Element 的 Ord 顺序排列。
--
-- 如果 Element 定义顺序为：
-- H | O | S | Cl | Zn | Fe | Cu
--
-- 那么结果是：
-- H < Cl < Zn
--------------------------------------------------

expectedElementBasis :: [Element]

expectedElementBasis =
    [
        H
        ,Cl
        ,Zn
    ]


--------------------------------------------------
-- 预期物种基
--------------------------------------------------

expectedSpeciesBasis :: [SpeciesVariable]

expectedSpeciesBasis =
    [
        SpeciesVariable
            {
                variableIndexIR = 1
                ,sideIR = ReactantSide
                ,formulaIR = formulaZn
            }

        ,SpeciesVariable
            {
                variableIndexIR = 2
                ,sideIR = ReactantSide
                ,formulaIR = formulaHCl
            }

        ,SpeciesVariable
            {
                variableIndexIR = 3
                ,sideIR = ProductSide
                ,formulaIR = formulaZnCl2
            }

        ,SpeciesVariable
            {
                variableIndexIR = 4
                ,sideIR = ProductSide
                ,formulaIR = formulaH2
            }
    ]


--------------------------------------------------
-- 预期计量矩阵
--
-- 元素行顺序：
-- H, Cl, Zn
--
-- 物种列顺序：
-- Zn, HCl, ZnCl2, H2
--
-- 反应物为负，生成物为正。
--------------------------------------------------

expectedMatrix :: [[Integer]]

expectedMatrix =
    [
        [ 0, -1, 0, 2 ]
        ,[ 0, -1, 2, 0 ]
        ,[ -1, 0, 1, 0 ]
    ]


--------------------------------------------------
-- 主测试
--------------------------------------------------

main :: IO ()

main = do

    let actualIR =
            buildBalanceIR testReaction

    putStrLn
        "========== Balance IR tests =========="

    assertEqual
        "element basis"
        expectedElementBasis
        (elementBasisIR actualIR)

    assertEqual
        "species basis"
        expectedSpeciesBasis
        (speciesBasisIR actualIR)

    assertEqual
        "stoichiometric matrix"
        expectedMatrix
        (stoichiometricMatrixIR actualIR)

    putStrLn ""

    putStrLn
        "Generated BalanceIR:"

    print actualIR