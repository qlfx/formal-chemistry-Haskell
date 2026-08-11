module Main where

import Chemistry.Workers.Lexer
    ( lexer )

import Chemistry.Workers.Parser
    ( parseReaction )

import Chemistry.Workers.Analyzer
    ( analyzeReaction )

import Chemistry.Workers.Evaluator
    ( evaluate )

import Chemistry.Workers.Render
    ( renderReactionResult )


main :: IO ()
main =
    case lexer "Zn + Fe(OH)2" of
        Left lexerError ->
            print lexerError

        Right tokens ->
            case parseReaction tokens of
                Left parserError ->
                    putStrLn parserError

                Right reactionAST ->
                    case analyzeReaction reactionAST of
                        Left semanticError ->
                            print semanticError

                        Right reactionInput ->
                            print
                                (renderReactionResult
                                    (evaluate reactionInput))