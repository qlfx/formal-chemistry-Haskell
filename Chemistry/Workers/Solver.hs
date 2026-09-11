module Chemistry.Workers.Solver
    ( haskellSolver
    , SolvedCoefficients(..)
    ) where

import Chemistry.Definitions.BalanceIR
    ( BalanceIR(..)
    )
import qualified Chemistry.Math.GaussianElimination as Math

newtype SolvedCoefficients =
    SolvedCoefficients [Integer]
    deriving (Eq, Show)

haskellSolver :: BalanceIR -> SolvedCoefficients
haskellSolver balanceIR =
    let matrix =
            Math.Matrix
                (map Math.Vector (stoichiometricMatrixIR balanceIR))
        Math.Vector coefficients = Math.findKernel matrix
    in
        SolvedCoefficients coefficients
