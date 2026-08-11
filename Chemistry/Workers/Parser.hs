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
import Chemistry.Definitions.Error(SyntaxError(..))

parseReaction :: [Token]  ->Either SyntaxError ReactionAST   --ReactionAST is actually a list of Species
parseReaction tokens = do
    (firstSpecies,rest1) <- parseSpecies tokens
    case rest1 of
        TokPlus : rest2 -> do
            (secondSpecies,rest3) <- parseSpecies rest2
            parseMoreSpecies [firstSpecies,secondSpecies] rest3
        
        TokEOF : _ -> Left InsufficientReactants

        token : _ -> Left
                        UnexpectedToken
                            {
                                expected = "'+' between reactants"
                                ,actual = token
                            }

        [] -> Left 
            UnexpectedEndOfInput
                {
                    expected = "'+' between reactants"
                }

parseSpecies :: [Token] -> Either SyntaxError (Species,[Token])
parseSpecies (TokNumber n : rest) = do

    (formula,remaining) <- parseFormula rest
    Right
        (
            Species
                {
                    coefficientAST = n
                    ,formulaAST = formula
                }
            ,remaining
        )

parseSpecies tokens = do

    (formula,remaining) <- parseFormula tokens
    Right
        ( Species
            { coefficientAST = 1
            , formulaAST = formula
            }
        , remaining
        )

parseMoreSpecies :: [Species] -> [Token] -> Either SyntaxError ReactionAST
parseMoreSpecies speciesList (TokPlus : rest)  = do
    (nextSpecies,remaining) <- parseSpecies rest
    parseMoreSpecies (speciesList ++ [nextSpecies]) remaining

parseMoreSpecies speciesList [TokEOF] = Right (ReactionAST speciesList)

parseMoreSpecies _ (token : _) = 
    Left 
        UnexpectedToken
            {
                expected = "'+' or end of reaction"
                ,actual = token
            }

parseMoreSpecies _ [] = 
    Left
        UnexpectedEndOfInput 
            {
                expected = "'+' or end of reaction"
            }

parseFormula :: [Token] -> Either SyntaxError (Formula,[Token])
parseFormula tokens  = do
    (firstPart,remaining) <- parseFormulaPart tokens
    parseMoreFormulaParts [firstPart] remaining

parseFormulaPart :: [Token] -> Either SyntaxError (FormulaPart,[Token])
parseFormulaPart (TokElement element : TokNumber n : rest)
    | n < 1 = Left (InvalidSubscript n)

    | otherwise = 
        Right 
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

parseFormulaPart (token : _) = Left UnexpectedToken
                                    {
                                        expected = "element or ("
                                        ,actual = token
                                    }

parseFormulaPart [] = 
    Left UnexpectedEndOfInput
            {
                expected = "element or ("
            }

parseGroupEnd :: Formula -> [Token] -> Either SyntaxError (FormulaPart,[Token])
parseGroupEnd innerFormula (TokRParen : TokNumber n : rest)
    | n < 1 = Left (InvalidSubscript n)
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

parseGroupEnd _ (TokEOF : _) = Left UnclosedParenthesis

parseGroupEnd _ [] = 
    Left UnclosedParenthesis

parseGroupEnd _ (token : _) = 
    Left
        UnexpectedToken
            {
                expected = "')'"
                ,actual = token
            }

--第四章的Follow集再回来看
parseMoreFormulaParts :: [FormulaPart] -> [Token] -> Either SyntaxError(Formula,[Token])
parseMoreFormulaParts parts tokens@(TokElement _ : _) = do
    (nextPart, remaining) <- parseFormulaPart tokens
    parseMoreFormulaParts (parts ++ [nextPart]) remaining
    
parseMoreFormulaParts parts tokens@(TokLParen : _) = do
    (nextPart, remaining) <- parseFormulaPart tokens
    parseMoreFormulaParts (parts ++ [nextPart]) remaining

parseMoreFormulaParts parts remaining@(TokPlus : _) =
    Right (Formula parts, remaining)

parseMoreFormulaParts parts remaining@(TokRParen : _) =
    Right (Formula parts, remaining)

parseMoreFormulaParts parts remaining@(TokEOF : _) =
    Right (Formula parts, remaining)

parseMoreFormulaParts _ (token : _) =
    Left
        UnexpectedToken
            { expected = "element, '(', '+', ')' or end of input"
            , actual = token
            }

parseMoreFormulaParts _ [] =
    Left
        UnexpectedEndOfInput
            { expected = "formula terminator"
            }