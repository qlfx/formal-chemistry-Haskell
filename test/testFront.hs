module Main where

import Chemistry.Workers.Lexer
    ( lexer )

import Chemistry.Workers.Parser
    ( parseReaction )

import Chemistry.Workers.Analyzer
    ( analyzeReaction
    , analyzeReactionAST
    )

import Chemistry.Definitions.Types
    ( Metal(..)
    , Acid(..)
    , Salt(..)
    , ReactionInput(..)
    )

import Chemistry.Definitions.Semantic
    ( SemanticSpecies(..)
    , ClassifiedReaction(..)
    )

import Chemistry.Definitions.Error


--------------------------------------------------
-- 联合测试错误类型
--------------------------------------------------

data FrontendError
    = LexicalError String
    | SyntaxError String
    | SemanticAnalysisError SemanticError
    deriving (Eq, Show)


--------------------------------------------------
-- 完整前端
--
-- String
-- → Lexer
-- → Parser
-- → Analyzer
-- → ReactionInput
--------------------------------------------------

compileFrontend
    :: String
    -> Either FrontendError ReactionInput

compileFrontend source =
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
                            Right reactionInput


--------------------------------------------------
-- 编译到 ClassifiedReaction
--
-- 用来检查：
-- coefficientSemantic
-- formulaSemantic
-- substanceSemantic
--------------------------------------------------

compileClassified
    :: String
    -> Either FrontendError ClassifiedReaction

compileClassified source =
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
                    case analyzeReactionAST reactionAST of

                        Left semanticError ->
                            Left
                                (SemanticAnalysisError
                                    semanticError)

                        Right classifiedReaction ->
                            Right classifiedReaction


--------------------------------------------------
-- 相等测试
--------------------------------------------------

assertEqual
    :: (Eq a, Show a)
    => String
    -> a
    -> a
    -> IO ()

assertEqual testName expected actual =
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
-- 检查错误发生在哪个阶段
--------------------------------------------------

assertLexicalError
    :: String
    -> Either FrontendError a
    -> IO ()

assertLexicalError testName result =
    case result of

        Left (LexicalError _) ->
            putStrLn
                ("[PASS] " ++ testName)

        _ -> do
            putStrLn
                ("[FAIL] " ++ testName)

            putStrLn
                ("  actual: " ++ showFrontendResult result)


assertSyntaxError
    :: String
    -> Either FrontendError a
    -> IO ()

assertSyntaxError testName result =
    case result of

        Left (SyntaxError _) ->
            putStrLn
                ("[PASS] " ++ testName)

        _ -> do
            putStrLn
                ("[FAIL] " ++ testName)

            putStrLn
                ("  actual: " ++ showFrontendResult result)


assertSemanticError
    :: String
    -> (SemanticError -> Bool)
    -> Either FrontendError a
    -> IO ()

assertSemanticError testName predicate result =
    case result of

        Left
            (SemanticAnalysisError semanticError)
                | predicate semanticError ->

                    putStrLn
                        ("[PASS] " ++ testName)

        _ -> do
            putStrLn
                ("[FAIL] " ++ testName)

            putStrLn
                ("  actual: " ++ showFrontendResult result)


--------------------------------------------------
-- 因为 a 未必实现 Show，
-- 这里只显示成功或错误类别
--------------------------------------------------

showFrontendResult
    :: Either FrontendError a
    -> String

showFrontendResult result =
    case result of

        Left frontendError ->
            show frontendError

        Right _ ->
            "Right <result>"


--------------------------------------------------
-- 具体语义错误判断
--------------------------------------------------

isUnsupportedFormula
    :: SemanticError
    -> Bool

isUnsupportedFormula
    (UnsupportedFormula _) =
    True

isUnsupportedFormula _ =
    False


isUnsupportedCombination
    :: SemanticError
    -> Bool

isUnsupportedCombination
    (UnsupportedReactionCombination _) =
    True

isUnsupportedCombination _ =
    False


isUnsupportedCount
    :: Int
    -> SemanticError
    -> Bool

isUnsupportedCount expectedCount semanticError =
    case semanticError of

        UnsupportedReactantCount actualCount ->
            actualCount == expectedCount

        _ ->
            False


--------------------------------------------------
-- 检查 ClassifiedReaction 中的第一个系数
--------------------------------------------------

assertFirstCoefficient
    :: String
    -> Int
    -> Either FrontendError ClassifiedReaction
    -> IO ()

assertFirstCoefficient
    testName
    expectedCoefficient
    result =

    case result of

        Right
            (ClassifiedReaction
                (firstSpecies : _)) ->

            assertEqual
                testName
                expectedCoefficient
                (coefficientSemantic firstSpecies)

        Right
            (ClassifiedReaction []) -> do

            putStrLn
                ("[FAIL] " ++ testName)

            putStrLn
                "  ClassifiedReaction is empty"

        Left frontendError -> do

            putStrLn
                ("[FAIL] " ++ testName)

            putStrLn
                ("  actual: " ++ show frontendError)


--------------------------------------------------
-- 主测试
--------------------------------------------------

main :: IO ()
main = do

    putStrLn
        "========== Successful cases =========="


    assertEqual
        "Zn + HCl"

        (Right
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl))

        (compileFrontend
            "Zn + HCl")


    assertEqual
        "HCl + Zn is normalized"

        (Right
            (ReactionInput_Metal_Acid
                MetalZn
                AcidHCl))

        (compileFrontend
            "HCl + Zn")


    assertEqual
        "Fe + H2SO4"

        (Right
            (ReactionInput_Metal_Acid
                MetalFe
                AcidH2SO4))

        (compileFrontend
            "Fe + H2SO4")


    assertEqual
        "Cu + ZnSO4"

        (Right
            (ReactionInput_Metal_Salt
                MetalCu
                ZnSO4))

        (compileFrontend
            "Cu + ZnSO4")


    assertEqual
        "ZnSO4 + Cu is normalized"

        (Right
            (ReactionInput_Metal_Salt
                MetalCu
                ZnSO4))

        (compileFrontend
            "ZnSO4 + Cu")


    assertFirstCoefficient
        "coefficient 2 is preserved"
        2
        (compileClassified
            "2Zn + HCl")


    putStrLn ""
    putStrLn
        "========== Error cases =========="


    assertLexicalError
        "unsupported element Ca"
        (compileFrontend
            "Ca + HCl")


    assertSyntaxError
        "missing species after +"
        (compileFrontend
            "Zn +")


    assertSyntaxError
        "coefficient zero is rejected by Parser"
        (compileFrontend
            "0Zn + HCl")


    assertSemanticError
        "Fe(OH)2 is syntactically valid but unsupported"
        isUnsupportedFormula
        (compileFrontend
            "Fe(OH)2 + HCl")


    assertSemanticError
        "Metal + Metal is unsupported"
        isUnsupportedCombination
        (compileFrontend
            "Zn + Fe")


    assertSemanticError
        "Acid + Acid is unsupported"
        isUnsupportedCombination
        (compileFrontend
            "HCl + H2SO4")


    assertSemanticError
        "three reactants are unsupported"
        (isUnsupportedCount 3)
        (compileFrontend
            "Zn + HCl + Fe")