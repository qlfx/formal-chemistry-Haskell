module Main where

import Control.Monad
    ( unless
    )
import System.Exit
    ( exitFailure
    )

import Chemistry.Definitions.BalanceIR
    ( BalanceIR(..)
    )
import Chemistry.Math.GaussianElimination
    ( Matrix(..)
    , Vector(..)
    , findKernel
    )
import Chemistry.Workers.Solver
    ( SolvedCoefficients(..)
    , haskellSolver
    )

type Test = IO Bool

main :: IO ()
main = do
    putStrLn "Gaussian elimination and kernel tests"
    putStrLn ""

    results <-
        sequence
            [ ordinaryOneDimensionalKernelTest
            , pivotRowSwapTest
            , fractionalIntermediateResultTest
            , zeroKernelTest
            , zincHydrochloricAcidTest
            ]

    let passed = length (filter id results)
        total = length results

    putStrLn ""
    putStrLn (show passed ++ "/" ++ show total ++ " tests passed")
    unless (and results) exitFailure

assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> Test
assertEqual testName expected actual =
    if actual == expected
        then do
            putStrLn ("[PASS] " ++ testName)
            pure True
        else do
            putStrLn ("[FAIL] " ++ testName)
            putStrLn ("       expected: " ++ show expected)
            putStrLn ("       actual:   " ++ show actual)
            pure False

matrix :: [[Integer]] -> Matrix Integer
matrix = Matrix . map Vector

ordinaryOneDimensionalKernelTest :: Test
ordinaryOneDimensionalKernelTest =
    assertEqual
        "ordinary one-dimensional kernel"
        (Vector [-1, 1])
        (findKernel (matrix [[1, 1]]))

pivotRowSwapTest :: Test
pivotRowSwapTest =
    assertEqual
        "zero pivot causes a row swap"
        (Vector [1, 1, 1])
        (findKernel
            (matrix
                [ [0, 1, -1]
                , [1, 0, -1]
                ]
            )
        )

fractionalIntermediateResultTest :: Test
fractionalIntermediateResultTest =
    assertEqual
        "fractional elimination is exact"
        (Vector [1, -1, 1])
        (findKernel
            (matrix
                [ [2, 1, -1]
                , [1, 1, 0]
                ]
            )
        )

zeroKernelTest :: Test
zeroKernelTest =
    assertEqual
        "full-column-rank matrix has only the zero vector"
        (Vector [0, 0])
        (findKernel
            (matrix
                [ [1, 2]
                , [3, 4]
                ]
            )
        )

zincHydrochloricAcidTest :: Test
zincHydrochloricAcidTest =
    let balanceIR =
            BalanceIR
                { elementBasisIR = []
                , speciesBasisIR = []
                , stoichiometricMatrixIR =
                    [ [0, -1, 0, 2]
                    , [0, -1, 2, 0]
                    , [-1, 0, 1, 0]
                    ]
                }
    in
        assertEqual
            "Zn + HCl -> ZnCl2 + H2"
            (SolvedCoefficients [1, 2, 1, 1])
            (haskellSolver balanceIR)
