module Main where

import Control.Monad
    ( unless
    )
import System.Exit
    ( exitFailure
    )

import Chemistry.Definitions.Error
    ( CompileError(..)
    , SemanticError(..)
    )
import Chemistry.Definitions.Types
    ( Acid(..)
    , ElementalSubstance(..)
    , Metal(..)
    , ReactionInput(..)
    , ReactionResult(..)
    , Salt(..)
    )
import Chemistry.Interface.Compiler
    ( analyzeSource
    , compileReaction
    , evaluateSource
    )

--------------------------------------------------
-- Test runner
--------------------------------------------------

type Test = IO Bool

main :: IO ()
main = do
    putStrLn "Chemistry compiler integration tests"
    putStrLn ""

    results <-
        sequence
            [ analyzeMetalAcidTest
            , analyzeReversedReactantsTest
            , analyzeMetalSaltTest
            , evaluateMetalAcidReactionTest
            , evaluateMetalAcidNoReactionTest
            , evaluateMetalSaltReactionTest
            , evaluateMetalSaltNoReactionTest
            , compileMetalAcidReactionTest
            , compileMetalAcidNoReactionTest
            , compileMetalSaltReactionTest
            , compileMetalSaltNoReactionTest
            , lexicalFailureTest
            , syntaxFailureTest
            , unsupportedFormulaTest
            , unsupportedCombinationTest
            , unsupportedReactantCountTest
            , earliestFailureWinsTest
            ]

    let passed = length (filter id results)
        total = length results

    putStrLn ""
    putStrLn
        (show passed ++ "/" ++ show total ++ " tests passed")

    unless (and results) exitFailure

assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> Test
assertEqual testName expected actual =
    if actual == expected
        then pass testName
        else failWith
            testName
            ( "expected: " ++ show expected
                ++ "\n         actual:   " ++ show actual
            )

assertCompileError
    :: Show a
    => String
    -> (CompileError -> Bool)
    -> Either CompileError a
    -> Test
assertCompileError testName predicate result =
    case result of
        Left compileError
            | predicate compileError ->
                pass testName

        _ ->
            failWith
                testName
                ("unexpected result: " ++ showEither result)

showEither
    :: Show a
    => Either CompileError a
    -> String
showEither result =
    case result of
        Left compileError ->
            "Left " ++ show compileError

        Right value ->
            "Right " ++ show value

pass :: String -> Test
pass testName = do
    putStrLn ("[PASS] " ++ testName)
    pure True

failWith :: String -> String -> Test
failWith testName details = do
    putStrLn ("[FAIL] " ++ testName)
    putStrLn ("         " ++ details)
    pure False

--------------------------------------------------
-- Compiler error predicates
--------------------------------------------------

isLexicalFailure :: CompileError -> Bool
isLexicalFailure (LexicalFailure _) = True
isLexicalFailure _ = False

isSyntaxFailure :: CompileError -> Bool
isSyntaxFailure (SyntaxFailure _) = True
isSyntaxFailure _ = False

isUnsupportedFormulaFailure :: CompileError -> Bool
isUnsupportedFormulaFailure
    (SemanticFailure (UnsupportedFormula _)) =
        True
isUnsupportedFormulaFailure _ = False

isUnsupportedCombinationFailure :: CompileError -> Bool
isUnsupportedCombinationFailure
    (SemanticFailure (UnsupportedReactionCombination _)) =
        True
isUnsupportedCombinationFailure _ = False

isUnsupportedCountFailure :: Int -> CompileError -> Bool
isUnsupportedCountFailure expectedCount compileError =
    case compileError of
        SemanticFailure
            (UnsupportedReactantCount actualCount) ->
                actualCount == expectedCount

        _ ->
            False

--------------------------------------------------
-- analyzeSource tests
--------------------------------------------------

analyzeMetalAcidTest :: Test
analyzeMetalAcidTest =
    assertEqual
        "analyzeSource: metal + acid"
        (Right
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl
            )
        )
        (analyzeSource "Zn + HCl")

analyzeReversedReactantsTest :: Test
analyzeReversedReactantsTest =
    assertEqual
        "analyzeSource: acid + metal is normalized"
        (Right
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl
            )
        )
        (analyzeSource "HCl + Zn")

analyzeMetalSaltTest :: Test
analyzeMetalSaltTest =
    assertEqual
        "analyzeSource: metal + salt"
        (Right
            (ReactionInput_Metal_Salt
                MetalZn
                CuSO4
            )
        )
        (analyzeSource "Zn + CuSO4")

--------------------------------------------------
-- evaluateSource tests
--------------------------------------------------

evaluateMetalAcidReactionTest :: Test
evaluateMetalAcidReactionTest =
    assertEqual
        "evaluateSource: Zn reacts with HCl"
        (Right
            (ReactionOccurs_Metal_Acid
                { metalAcidReactantMetal = MetalZn
                , metalAcidReactantAcid = AcidHCl
                , metalAcidProductSalt = ZnCl2
                , metalAcidProductElementalSubstance =
                    ElementalHydrogen
                })
        )
        (evaluateSource "Zn + HCl")

evaluateMetalAcidNoReactionTest :: Test
evaluateMetalAcidNoReactionTest =
    assertEqual
        "evaluateSource: no-reaction is still Right"
        (Right
            (NoReaction_Metal_Acid
                { noReactionMetalAcidMetal = MetalCu
                , noReactionMetalAcidAcid = AcidHCl
                })
        )
        (evaluateSource "Cu + HCl")

evaluateMetalSaltReactionTest :: Test
evaluateMetalSaltReactionTest =
    assertEqual
        "evaluateSource: Zn displaces Cu"
        (Right
            (ReactionOccurs_Metal_Salt
                { metalSaltReactantMetal = MetalZn
                , metalSaltReactantSalt = CuSO4
                , metalSaltProductElementalSubstance =
                    ElementalMetal MetalCu
                , metalSaltProductSalt = ZnSO4
                })
        )
        (evaluateSource "Zn + CuSO4")

evaluateMetalSaltNoReactionTest :: Test
evaluateMetalSaltNoReactionTest =
    assertEqual
        "evaluateSource: Cu cannot displace Zn"
        (Right
            (NoReaction_Metal_Salt
                { noReactionMetalSaltMetal = MetalCu
                , noReactionMetalSaltSalt = ZnSO4
                })
        )
        (evaluateSource "Cu + ZnSO4")

--------------------------------------------------
-- compileReaction tests
--------------------------------------------------

compileMetalAcidReactionTest :: Test
compileMetalAcidReactionTest =
    assertEqual
        "compileReaction: renders metal + acid"
        (Right "Zn+HCl->ZnCl2+H2")
        (compileReaction "Zn + HCl")

compileMetalAcidNoReactionTest :: Test
compileMetalAcidNoReactionTest =
    assertEqual
        "compileReaction: renders acid no-reaction"
        (Right "Cu+HCl->No Reaction")
        (compileReaction "Cu + HCl")

compileMetalSaltReactionTest :: Test
compileMetalSaltReactionTest =
    assertEqual
        "compileReaction: renders metal displacement"
        (Right "Zn+CuSO4->Cu+ZnSO4")
        (compileReaction "Zn + CuSO4")

compileMetalSaltNoReactionTest :: Test
compileMetalSaltNoReactionTest =
    assertEqual
        "compileReaction: renders salt no-reaction"
        (Right "Cu+ZnSO4->No Reaction")
        (compileReaction "Cu + ZnSO4")

--------------------------------------------------
-- Error propagation tests
--------------------------------------------------

lexicalFailureTest :: Test
lexicalFailureTest =
    assertCompileError
        "Compiler wraps a Lexer error"
        isLexicalFailure
        (compileReaction "Zn + X")

syntaxFailureTest :: Test
syntaxFailureTest =
    assertCompileError
        "Compiler wraps a Parser error"
        isSyntaxFailure
        (compileReaction "Zn")

unsupportedFormulaTest :: Test
unsupportedFormulaTest =
    assertCompileError
        "Compiler wraps UnsupportedFormula"
        isUnsupportedFormulaFailure
        (compileReaction "H2 + HCl")

unsupportedCombinationTest :: Test
unsupportedCombinationTest =
    assertCompileError
        "Compiler wraps UnsupportedReactionCombination"
        isUnsupportedCombinationFailure
        (compileReaction "HCl + H2SO4")

unsupportedReactantCountTest :: Test
unsupportedReactantCountTest =
    assertCompileError
        "Compiler rejects three reactants"
        (isUnsupportedCountFailure 3)
        (compileReaction "Zn + HCl + Cu")

earliestFailureWinsTest :: Test
earliestFailureWinsTest =
    assertCompileError
        "Lexer failure short-circuits later phases"
        isLexicalFailure
        (compileReaction "X + HCl + Cu")