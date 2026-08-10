module Main where

import Chemistry.Workers.Parser
import Chemistry.Definitions.Token
import Chemistry.Definitions.Types
import Chemistry.Definitions.AST
import Chemistry.Workers.Lexer
import Chemistry.Definitions.Error

getToken :: Either String [Token] -> [Token]
getToken result = 
    case result of 
        Right tokens -> tokens

        _  -> []

main :: IO ()
main = do

    print $
        parseFormulaPart
            [ TokElement H
            , TokNumber 2
            , TokEOF
            ]

    print $
        parseFormula
            [ TokElement H
            , TokNumber 2
            , TokElement S
            , TokElement O
            , TokNumber 4
            , TokEOF
            ]

    print $
        parseFormula
            [ TokElement Fe
            , TokLParen
            , TokElement O
            , TokElement H
            , TokRParen
            , TokNumber 2
            , TokEOF
            ]

    print $
        parseReaction
            [ TokElement Zn
            , TokPlus
            , TokElement H
            , TokElement Cl
            , TokEOF
            ]

    print (lexer "HCl + Zn" >>= parseReaction)
