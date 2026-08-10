{-
Grammar:

reaction
    → species ('+' species)+

species
    → number? formula

formula
    → formulaPart+

formulaPart
    → element number?
    | '(' formula ')' number?

Main parsers:

reaction     -> parseReaction
species      -> parseSpecies
formula      -> parseFormula
formulaPart  -> parseFormulaPart

Control helpers:

parseMoreSpecies
    -> repetition

parseMoreFormulaParts
    -> repetition

parseGroupEnd
    -> sequence + optional

-}
module Chemistry.Workers.Parser
    ( parseReaction
    , parseFormula
    , parseSpecies
    ,parseFormulaPart
    ) where

import Chemistry.Definitions.Types
import Chemistry.Definitions.AST
import Chemistry.Definitions.Token

parseReaction :: [Token]  ->Either String ReactionAST   --ReactionAST is actually a list of Species
parseReaction tokens = do
    (firstSpecies,rest1) <- parseSpecies tokens
    case rest1 of
        TokPlus : rest2 -> do
            (secondSpecies,rest3) <- parseSpecies rest2
            parseMoreSpecies [firstSpecies,secondSpecies] rest3
        
        _ -> Left "反应至少需要两个反应物，并用 + 分隔"

parseSpecies :: [Token] -> Either String (Species,[Token])
parseSpecies
    (TokNumber n : rest)

    | n < 1 =
        Left "化学计量系数必须大于零"

    | otherwise = do

        (formula, remaining) <-
            parseFormula rest

        Right
            ( Species
                { coefficientAST = n
                , formulaAST = formula
                }
            , remaining
            )

parseSpecies tokens = do

    (formula,remaining) <- 
        parseFormula tokens
        
    Right
        ( Species
            { coefficientAST = 1
            , formulaAST = formula
            }
        , remaining
        )

parseMoreSpecies :: [Species] -> [Token] -> Either String ReactionAST
parseMoreSpecies speciesList (TokPlus : rest)  = do
    (nextSpecies,remaining) <- parseSpecies rest
    parseMoreSpecies (speciesList ++ [nextSpecies]) remaining

parseMoreSpecies speciesList [TokEOF] = Right (ReactionAST speciesList)

parseMoreSpecies _ remaining = Left ("反应式存在无法解析的Token：" ++ show remaining)

parseFormula :: [Token] -> Either String (Formula,[Token])
parseFormula tokens  = do
    (firstPart,remaining) <- parseFormulaPart tokens
    parseMoreFormulaParts [firstPart] remaining

parseFormulaPart :: [Token] -> Either String (FormulaPart,[Token])
parseFormulaPart (TokElement element : TokNumber n : rest)
    | n < 1 = Left "元素下标必须大于0"
    | otherwise = Right 
        (
        Atom element n
        ,rest
        )

parseFormulaPart (TokElement element : rest) = 
    Right 
        (
            Atom element 1
            ,rest
        )

parseFormulaPart (TokLParen : rest) = do
    (innerFormula,remaining) <- parseFormula rest
    parseGroupEnd innerFormula remaining

parseFormulaPart tokens = Left ("期待左括号或元素，但得到：" ++ show tokens)

parseGroupEnd :: Formula -> [Token] -> Either String (FormulaPart,[Token])
parseGroupEnd innerFormula (TokRParen : TokNumber n : rest)
    | n < 1 = Left "下标必须大于0"
    | otherwise = Right 
        (
            Group innerFormula n
            ,rest
        )
parseGroupEnd innerFormula (TokRParen : rest) = 
    Right 
        (
            Group innerFormula 1
            ,rest
        )

parseGroupEnd _ remaining = Left ("左括号没有对应的括号，剩余：" ++ show remaining)


--第四章的Follow集再回来看
parseMoreFormulaParts :: [FormulaPart] -> [Token] -> Either String (Formula,[Token])
parseMoreFormulaParts
    parts
    tokens@(TokElement _ : _) = do

    (nextPart, remaining) <-
        parseFormulaPart tokens

    parseMoreFormulaParts
        (parts ++ [nextPart])
        remaining
    
parseMoreFormulaParts
    parts
    tokens@(TokLParen : _) = do

    (nextPart, remaining) <-
        parseFormulaPart tokens

    parseMoreFormulaParts
        (parts ++ [nextPart])
        remaining

parseMoreFormulaParts
    parts
    remaining =

    Right
        ( Formula parts
        , remaining
        )