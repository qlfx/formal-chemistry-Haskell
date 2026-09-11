module Chemistry.Math.GaussianElimination
    ( Vector(..)
    , Matrix(..)
    , findKernel
    , matrixDealer
    , getZero
    , disappear
    , kernelFromEchelon
    ) where

import Data.List
    ( findIndex
    , foldl'
    )
import Data.Ratio
    ( denominator
    , numerator
    )

-- | A mathematical vector. The Math layer deliberately does not know about
-- chemical formulae, species, reactions, or BalanceIR.
newtype Vector a =
    Vector [a]
    deriving (Eq, Show)

-- | A matrix represented as a list of row vectors.
newtype Matrix a =
    Matrix [Vector a]
    deriving (Eq, Show)

findKernel :: Matrix Integer -> Vector Integer
findKernel integerMatrix =
    let width = matrixWidth integerMatrix
        rationalMatrix = mapMatrix fromInteger integerMatrix
        echelonMatrix = matrixDealer rationalMatrix
        freeColumns = findFreeColumns width echelonMatrix
    in
        ensureRectangular integerMatrix `seq`
            normalizeIntegerVector (kernelFromEchelon width echelonMatrix)

-- | Control the row-echelon conversion one pivot column at a time.
-- If the current candidate pivot is zero, a non-zero row from the remaining
-- rows is selected and moved into the pivot position.
matrixDealer :: Matrix Rational -> Matrix Rational
matrixDealer matrix@(Matrix rows) =
    Matrix (dealColumn 0 (matrixWidth matrix) rows)

dealColumn :: Int -> Int -> [Vector Rational] -> [Vector Rational]
dealColumn _ _ [] = []
dealColumn pivotColumn width rows
    | pivotColumn >= width = rows
    | otherwise =
        case choosePivot pivotColumn rows of
            Nothing ->
                dealColumn (pivotColumn + 1) width rows

            Just (pivotRow, otherRows) ->
                let Matrix rowsWithZero =
                        getZero
                            pivotRow
                            (Matrix otherRows)
                in
                    pivotRow
                        : dealColumn
                            (pivotColumn + 1)
                            width
                            rowsWithZero

-- | Use one pivot row to make the same coordinate zero in every later row.
getZero :: Vector Rational -> Matrix Rational -> Matrix Rational
getZero pivotRow (Matrix rows) =
    Matrix (map (disappear pivotRow) rows)

-- | Perform one concrete elimination between two rows.
disappear :: Vector Rational -> Vector Rational -> Vector Rational
disappear (Vector pivotRow) target@(Vector targetRow) =
    case findIndex (/= 0) pivotRow of
        Nothing -> target
        Just pivotColumn
            | targetValue == 0 -> target
            | otherwise ->
                Vector
                    (zipWith
                        (\targetValue' pivotValue' ->
                            targetValue' - factor * pivotValue'
                        )
                        targetRow
                        pivotRow
                    )
          where
            pivotValue = pivotRow !! pivotColumn
            targetValue = targetRow !! pivotColumn
            factor = targetValue / pivotValue

-- | Construct one rational kernel vector from an already-echelon matrix.
-- This is deliberately separate from Gaussian elimination. The width is an
-- explicit argument because an empty row list cannot carry a column count.
kernelFromEchelon :: Int -> Matrix Rational -> Vector Rational
kernelFromEchelon width echelonMatrix@(Matrix rows) =
    case findFreeColumns width echelonMatrix of
        [] -> Vector (replicate width 0)
        selectedFreeColumn : _ ->
            Vector
                (foldl'
                    solvePivot
                    (replaceAt selectedFreeColumn 1 (replicate width 0))
                    (reverse (pivotRows rows))
                )
  where
    solvePivot values (pivotColumn, Vector row) =
        let knownSum =
                sum
                    [ coefficient * value
                    | (column, (coefficient, value)) <-
                        zip [0 ..] (zip row values)
                    , column /= pivotColumn
                    ]
            pivotValue = row !! pivotColumn
        in
            replaceAt pivotColumn (-knownSum / pivotValue) values

choosePivot :: Int -> [Vector Rational] -> Maybe (Vector Rational, [Vector Rational])
choosePivot pivotColumn rows =
    case break (hasNonZeroAt pivotColumn) rows of
        (_, []) -> Nothing
        (before, pivotRow : after) ->
            Just (pivotRow, before ++ after)

hasNonZeroAt :: (Eq a, Num a) => Int -> Vector a -> Bool
hasNonZeroAt column (Vector values) =
    values !! column /= 0

pivotRows :: [Vector Rational] -> [(Int, Vector Rational)]
pivotRows rows =
    [ (pivotColumn, row)
    | row@(Vector values) <- rows
    , Just pivotColumn <- [findIndex (/= 0) values]
    ]

findFreeColumns :: Int -> Matrix Rational -> [Int]
findFreeColumns width (Matrix rows) =
    filter (`notElem` pivotColumns) [0 .. width - 1]
  where
    pivotColumns = map fst (pivotRows rows)

normalizeIntegerVector :: Vector Rational -> Vector Integer
normalizeIntegerVector (Vector values) =
    let commonDenominator =
            foldl' lcm 1 (map denominator values)
        integers =
            map
                (\value ->
                    numerator
                        (value * fromInteger commonDenominator)
                )
                values
        commonDivisor =
            foldl' gcd 0 (map abs integers)
    in
        if commonDivisor == 0
            then Vector integers
            else Vector (map (`div` commonDivisor) integers)

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix function (Matrix rows) =
    Matrix
        [ Vector (map function values)
        | Vector values <- rows
        ]

matrixWidth :: Matrix a -> Int
matrixWidth (Matrix []) = 0
matrixWidth (Matrix (Vector firstRow : _)) = length firstRow

ensureRectangular :: Matrix a -> ()
ensureRectangular matrix@(Matrix rows)
    | all ((== matrixWidth matrix) . vectorLength) rows = ()
    | otherwise = error "findKernel: matrix rows must have equal length"

vectorLength :: Vector a -> Int
vectorLength (Vector values) = length values

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index newValue values =
    take index values ++ newValue : drop (index + 1) values