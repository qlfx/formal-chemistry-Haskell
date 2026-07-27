module Chemistry.Parser
    ( parseInputAST
    , parseFormula
    , parseSpecies
    ) where

import Data.Char
    ( isDigit
    , isLower
    , isSpace
    , isUpper
    )

import Chemistry.AST
    


removeSpaces :: String -> String
removeSpaces =
    filter (not . isSpace)


splitByPlus :: String -> [String]
splitByPlus input =
    case break (== '+') input of
        (left, []) ->
            [left]

        (left, _ : right) ->
            left : splitByPlus right


parseInputAST :: String -> Either String ReactionAST
parseInputAST rawInput = do
    let parts =
            splitByPlus
                (removeSpaces rawInput)

    if length parts < 2
        then Left "输入中至少需要两个反应物，并用 + 分隔"

        else if any null parts
            then Left "反应物不能为空"

            else do
                speciesList <-
                    traverse parseSpecies parts

                Right
                    (ReactionAST speciesList)


parseSpecies :: String -> Either String Species
parseSpecies input = do
    let (coefficientText, formulaText) =
            span isDigit input

        coefficientValue =
            case coefficientText of
                "" ->
                    1

                digits ->
                    read digits

    if coefficientValue < 1
        then Left "化学计量系数必须大于零"

        else if null formulaText
            then Left "系数后缺少化学式"

            else do
                parsedFormula <-
                    parseFormula formulaText

                Right
                    Species
                        { coefficientAST =
                            coefficientValue

                        , formulaAST =
                            parsedFormula
                        }


parseFormula :: String -> Either String Formula
parseFormula input = do
    (formulaValue, remaining) <-
        parseFormulaBody input

    case remaining of
        [] ->
            Right formulaValue

        ')' : _ ->
            Left "出现未匹配的右括号"

        _ ->
            Left
                ("无法解析剩余内容: "
                    ++ remaining)


parseFormulaBody
    :: String
    -> Either String (Formula, String)

parseFormulaBody =
    go []
  where
    go accumulated [] =
        finish accumulated []

    go accumulated input@(character : rest)
        | character == ')' =
            finish accumulated input

        | character == '(' = do
            (innerFormula, afterInner) <-
                parseFormulaBody rest

            case afterInner of
                ')' : afterClosing -> do
                    let (subscript, remaining) =
                            parsePositiveNumber
                                afterClosing

                    if subscript < 1
                        then
                            Left "括号下标必须大于零"

                        else
                            go
                                (Group
                                    innerFormula
                                    subscript
                                    : accumulated)
                                remaining

                _ ->
                    Left "左括号没有对应的右括号"

        | isUpper character = do
            let (lowercasePart, afterSymbol) =
                    span isLower rest

                symbol =
                    character : lowercasePart

                (subscript, remaining) =
                    parsePositiveNumber
                        afterSymbol

            element <-
                parseElement symbol

            if subscript < 1
                then
                    Left "元素下标必须大于零"

                else
                    go
                        (Atom element subscript
                            : accumulated)
                        remaining

        | otherwise =
            Left
                ("化学式中出现非法字符: "
                    ++ [character])

    finish [] _ =
        Left "化学式不能为空"

    finish accumulated remaining =
        Right
            ( Formula
                (reverse accumulated)
            , remaining
            )


parsePositiveNumber :: String -> (Int, String)
parsePositiveNumber input =
    case span isDigit input of
        ("", remaining) ->
            (1, remaining)

        (digits, remaining) ->
            (read digits, remaining)


parseElement :: String -> Either String Element
parseElement "H"  = Right H
parseElement "O"  = Right O
parseElement "S"  = Right S
parseElement "Cl" = Right Cl
parseElement "Zn" = Right Zn
parseElement "Fe" = Right Fe
parseElement "Cu" = Right Cu

parseElement symbol =
    Left
        ("暂不支持元素: "
            ++ symbol)