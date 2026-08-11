module Main where

import Chemistry.Workers.Lexer
    ( lexer )

import Chemistry.Workers.Parser
    ( parseReaction )

import Chemistry.Workers.Analyzer
    ( analyzeReaction )

import Chemistry.Workers.Evaluator
    ( evaluate
    , activityRank
    , moreActiveThan
    , comparisonOf
    , reactionOccurs
    , saltFromMetalAcid
    , replaceMetalInSalt
    )

import Chemistry.Definitions.Types
import Chemistry.Definitions.Error
import Chemistry.Definitions.Semantic

--------------------------------------------------
-- 完整前端错误
--------------------------------------------------

data CompilerError
    = LexicalError String
    | SyntaxError String
    | SemanticAnalysisError SemanticError
    deriving (Eq, Show)

--------------------------------------------------
-- 完整编译流程
--
-- String
-- → Lexer
-- → Parser
-- → Analyzer
-- → Evaluator
-- → ReactionResult
--------------------------------------------------

compileReaction
    :: String
    -> Either CompilerError ReactionResult

compileReaction source =
    case lexer source of

        Left lexerError ->
            Left
                (LexicalError lexerError)

        Right tokens ->
            case parseReaction tokens of

                Left parserError ->
                    Left
                        (SyntaxError parserError)

                Right reactionAST ->
                    case analyzeReaction reactionAST of

                        Left semanticError ->
                            Left
                                (SemanticAnalysisError
                                    semanticError)

                        Right reactionInput ->
                            Right
                                (evaluate reactionInput)


--------------------------------------------------
-- 通用相等测试
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
-- 检查错误阶段
--------------------------------------------------

assertLexicalError
    :: String
    -> Either CompilerError a
    -> IO ()

assertLexicalError testName result =
    case result of

        Left (LexicalError _) ->
            putStrLn
                ("[PASS] " ++ testName)

        _ ->
            putStrLn
                ("[FAIL] " ++ testName)


assertSyntaxError
    :: String
    -> Either CompilerError a
    -> IO ()

assertSyntaxError testName result =
    case result of

        Left (SyntaxError _) ->
            putStrLn
                ("[PASS] " ++ testName)

        _ ->
            putStrLn
                ("[FAIL] " ++ testName)


assertSemanticError
    :: String
    -> Either CompilerError a
    -> IO ()

assertSemanticError testName result =
    case result of

        Left (SemanticAnalysisError _) ->
            putStrLn
                ("[PASS] " ++ testName)

        _ ->
            putStrLn
                ("[FAIL] " ++ testName)


--------------------------------------------------
-- 预期结果：金属与酸发生反应
--------------------------------------------------

expectedZnHCl
    :: ReactionResult

expectedZnHCl =
    ReactionOccurs_Metal_Acid
        {
            metalAcidReactantMetal =
                MetalZn

            ,metalAcidReactantAcid =
                AcidHCl

            ,metalAcidProductSalt =
                ZnCl2

            ,metalAcidProductElementalSubstance =
                ElementalHydrogen
        }


expectedFeSulfuricAcid
    :: ReactionResult

expectedFeSulfuricAcid =
    ReactionOccurs_Metal_Acid
        {
            metalAcidReactantMetal =
                MetalFe

            ,metalAcidReactantAcid =
                AcidH2SO4

            ,metalAcidProductSalt =
                FeSO4

            ,metalAcidProductElementalSubstance =
                ElementalHydrogen
        }


--------------------------------------------------
-- 预期结果：金属与酸不反应
--------------------------------------------------

expectedCuHCl
    :: ReactionResult

expectedCuHCl =
    NoReaction_Metal_Acid
        {
            noReactionMetalAcidMetal =
                MetalCu

            ,noReactionMetalAcidAcid =
                AcidHCl
        }


--------------------------------------------------
-- 预期结果：金属与盐发生反应
--------------------------------------------------

expectedZnCuSO4
    :: ReactionResult

expectedZnCuSO4 =
    ReactionOccurs_Metal_Salt
        {
            metalSaltReactantMetal =
                MetalZn

            ,metalSaltReactantSalt =
                CuSO4

            ,metalSaltProductElementalSubstance =
                ElementalMetal MetalCu

            ,metalSaltProductSalt =
                ZnSO4
        }


expectedFeCuCl2
    :: ReactionResult

expectedFeCuCl2 =
    ReactionOccurs_Metal_Salt
        {
            metalSaltReactantMetal =
                MetalFe

            ,metalSaltReactantSalt =
                CuCl2

            ,metalSaltProductElementalSubstance =
                ElementalMetal MetalCu

            ,metalSaltProductSalt =
                FeCl2
        }


--------------------------------------------------
-- 预期结果：金属与盐不反应
--------------------------------------------------

expectedCuZnSO4
    :: ReactionResult

expectedCuZnSO4 =
    NoReaction_Metal_Salt
        {
            noReactionMetalSaltMetal =
                MetalCu

            ,noReactionMetalSaltSalt =
                ZnSO4
        }


expectedFeZnSO4
    :: ReactionResult

expectedFeZnSO4 =
    NoReaction_Metal_Salt
        {
            noReactionMetalSaltMetal =
                MetalFe

            ,noReactionMetalSaltSalt =
                ZnSO4
        }


--------------------------------------------------
-- 主测试
--------------------------------------------------

main :: IO ()
main = do

    putStrLn
        "========== Activity rule tests =========="


    assertEqual
        "activityRank Zn"
        4
        (activityRank
            (ElementalMetal MetalZn))


    assertEqual
        "activityRank Fe"
        3
        (activityRank
            (ElementalMetal MetalFe))


    assertEqual
        "activityRank Hydrogen"
        2
        (activityRank
            ElementalHydrogen)


    assertEqual
        "activityRank Cu"
        1
        (activityRank
            (ElementalMetal MetalCu))


    assertEqual
        "Zn is more active than Hydrogen"
        True
        (moreActiveThan
            (ElementalMetal MetalZn)
            ElementalHydrogen)


    assertEqual
        "Cu is not more active than Hydrogen"
        False
        (moreActiveThan
            (ElementalMetal MetalCu)
            ElementalHydrogen)


    assertEqual
        "Fe is more active than Cu"
        True
        (moreActiveThan
            (ElementalMetal MetalFe)
            (ElementalMetal MetalCu))


    assertEqual
        "Fe is not more active than Zn"
        False
        (moreActiveThan
            (ElementalMetal MetalFe)
            (ElementalMetal MetalZn))


    putStrLn ""
    putStrLn
        "========== comparisonOf tests =========="


    assertEqual
        "Metal-Acid comparison"

        (
            ElementalMetal MetalZn
            ,ElementalHydrogen
        )

        (comparisonOf
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl))


    assertEqual
        "Metal-Salt comparison"

        (
            ElementalMetal MetalZn
            ,ElementalMetal MetalCu
        )

        (comparisonOf
            (ReactionInput_Metal_Salt
                MetalZn
                CuSO4))


    putStrLn ""
    putStrLn
        "========== reactionOccurs tests =========="


    assertEqual
        "Zn + HCl can react"
        True
        (reactionOccurs
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl))


    assertEqual
        "Cu + HCl cannot react"
        False
        (reactionOccurs
            (ReactionInput_Metal_Acid
                MetalCu
                AcidHCl))


    assertEqual
        "Zn + CuSO4 can react"
        True
        (reactionOccurs
            (ReactionInput_Metal_Salt
                MetalZn
                CuSO4))


    assertEqual
        "Cu + ZnSO4 cannot react"
        False
        (reactionOccurs
            (ReactionInput_Metal_Salt
                MetalCu
                ZnSO4))


    putStrLn ""
    putStrLn
        "========== Product rule tests =========="


    assertEqual
        "Zn and HCl produce ZnCl2"
        ZnCl2
        (saltFromMetalAcid
            MetalZn
            AcidHCl)


    assertEqual
        "Fe and H2SO4 produce FeSO4"
        FeSO4
        (saltFromMetalAcid
            MetalFe
            AcidH2SO4)


    assertEqual
        "replace Cu in CuSO4 with Zn"
        ZnSO4
        (replaceMetalInSalt
            MetalZn
            CuSO4)


    assertEqual
        "replace Cu in CuCl2 with Fe"
        FeCl2
        (replaceMetalInSalt
            MetalFe
            CuCl2)


    putStrLn ""
    putStrLn
        "========== Evaluator unit tests =========="


    assertEqual
        "evaluate Zn + HCl"
        expectedZnHCl
        (evaluate
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl))


    assertEqual
        "evaluate Cu + HCl"
        expectedCuHCl
        (evaluate
            (ReactionInput_Metal_Acid
                MetalCu
                AcidHCl))


    assertEqual
        "evaluate Zn + CuSO4"
        expectedZnCuSO4
        (evaluate
            (ReactionInput_Metal_Salt
                MetalZn
                CuSO4))


    assertEqual
        "evaluate Fe + CuCl2"
        expectedFeCuCl2
        (evaluate
            (ReactionInput_Metal_Salt
                MetalFe
                CuCl2))


    assertEqual
        "evaluate Cu + ZnSO4"
        expectedCuZnSO4
        (evaluate
            (ReactionInput_Metal_Salt
                MetalCu
                ZnSO4))


    putStrLn ""
    putStrLn
        "========== Complete pipeline tests =========="


    assertEqual
        "String pipeline: Zn + HCl"
        (Right expectedZnHCl)
        (compileReaction
            "Zn + HCl")


    assertEqual
        "String pipeline: HCl + Zn is normalized"
        (Right expectedZnHCl)
        (compileReaction
            "HCl + Zn")


    assertEqual
        "String pipeline: Fe + H2SO4"
        (Right expectedFeSulfuricAcid)
        (compileReaction
            "Fe + H2SO4")


    assertEqual
        "String pipeline: Cu + HCl"
        (Right expectedCuHCl)
        (compileReaction
            "Cu + HCl")


    assertEqual
        "String pipeline: Zn + CuSO4"
        (Right expectedZnCuSO4)
        (compileReaction
            "Zn + CuSO4")


    assertEqual
        "String pipeline: Cu + ZnSO4"
        (Right expectedCuZnSO4)
        (compileReaction
            "Cu + ZnSO4")


    assertEqual
        "String pipeline: Fe + CuCl2"
        (Right expectedFeCuCl2)
        (compileReaction
            "Fe + CuCl2")


    assertEqual
        "String pipeline: Fe + ZnSO4"
        (Right expectedFeZnSO4)
        (compileReaction
            "Fe + ZnSO4")


    putStrLn ""
    putStrLn
        "========== Frontend error tests =========="


    assertLexicalError
        "unsupported element Ca"
        (compileReaction
            "Ca + HCl")


    assertSyntaxError
        "missing species after plus"
        (compileReaction
            "Zn +")


    assertSyntaxError
        "coefficient zero"
        (compileReaction
            "0Zn + HCl")


    assertSemanticError
        "unsupported formula Fe(OH)2"
        (compileReaction
            "Fe(OH)2 + HCl")


    assertSemanticError
        "unsupported Metal + Metal"
        (compileReaction
            "Zn + Fe")


    assertSemanticError
        "unsupported three reactants"
        (compileReaction
            "Zn + HCl + Fe")