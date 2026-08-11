module Main where

import Chemistry.Definitions.AST
    ( Formula(..)
    , FormulaPart(..)
    , ReactionAST(..)
    , Species(..)
    )
import Chemistry.Definitions.Error
    ( SyntaxError(..) )
import Chemistry.Definitions.Token
    ( Token(..) )
import Chemistry.Definitions.Types
    ( Element(..) )
import Chemistry.Workers.Parser
    ( parseFormula
    , parseFormulaPart
    , parseReaction
    , parseSpecies
    )
import Control.Monad
    ( unless )
import System.Exit
    ( exitFailure )

-- Run from the project root with:
--
--   runghc -i. test/TestParserBoundary.hs
--
-- This file deliberately uses only base, matching the project's current setup.

main :: IO ()
main = do
    results <- sequence
        [ -- parseFormulaPart: element and subscript boundaries
          check
            "formulaPart: element without subscript defaults to 1"
            (Right (Atom H 1, [TokEOF]))
            (parseFormulaPart [TokElement H, TokEOF])

        , check
            "formulaPart: the smallest valid subscript is 1"
            (Right (Atom H 1, [TokEOF]))
            (parseFormulaPart [TokElement H, TokNumber 1, TokEOF])

        , check
            "formulaPart: subscript 0 is rejected"
            (Left (InvalidSubscript 0))
            (parseFormulaPart [TokElement H, TokNumber 0, TokEOF])

        , check
            "formulaPart: a negative subscript is rejected"
            (Left (InvalidSubscript (-1)))
            (parseFormulaPart [TokElement H, TokNumber (-1), TokEOF])

        , check
            "formulaPart: group without subscript defaults to 1"
            ( Right
                ( Group (Formula [Atom O 1, Atom H 1]) 1
                , [TokEOF]
                )
            )
            ( parseFormulaPart
                [ TokLParen
                , TokElement O
                , TokElement H
                , TokRParen
                , TokEOF
                ]
            )

        , check
            "formulaPart: group subscript is retained"
            ( Right
                ( Group (Formula [Atom O 1, Atom H 1]) 2
                , [TokEOF]
                )
            )
            ( parseFormulaPart
                [ TokLParen
                , TokElement O
                , TokElement H
                , TokRParen
                , TokNumber 2
                , TokEOF
                ]
            )

        , check
            "formulaPart: group subscript 0 is rejected"
            (Left (InvalidSubscript 0))
            ( parseFormulaPart
                [ TokLParen
                , TokElement O
                , TokElement H
                , TokRParen
                , TokNumber 0
                , TokEOF
                ]
            )

        , check
            "formulaPart: EOF cannot begin a formula part"
            ( Left
                UnexpectedToken
                    { expected = "element or ("
                    , actual = TokEOF
                    }
            )
            (parseFormulaPart [TokEOF])

        , check
            "formulaPart: an empty token stream is reported separately"
            ( Left
                UnexpectedEndOfInput
                    { expected = "element or (" }
            )
            (parseFormulaPart [])

        , check
            "formulaPart: a number cannot begin a formula part"
            ( Left
                UnexpectedToken
                    { expected = "element or ("
                    , actual = TokNumber 2
                    }
            )
            (parseFormulaPart [TokNumber 2, TokEOF])

          -- parseFormula: FIRST(formulaPart) and FOLLOW(formula)
        , check
            "formula: a single atom may end before EOF"
            (Right (Formula [Atom H 1], [TokEOF]))
            (parseFormula [TokElement H, TokEOF])

        , check
            "formula: repeated formula parts are accumulated"
            ( Right
                ( Formula [Atom H 2, Atom S 1, Atom O 4]
                , [TokEOF]
                )
            )
            ( parseFormula
                [ TokElement H
                , TokNumber 2
                , TokElement S
                , TokElement O
                , TokNumber 4
                , TokEOF
                ]
            )

        , check
            "formula: TokPlus is a legal FOLLOW token and is not consumed"
            (Right (Formula [Atom H 2], [TokPlus, TokElement O, TokEOF]))
            ( parseFormula
                [ TokElement H
                , TokNumber 2
                , TokPlus
                , TokElement O
                , TokEOF
                ]
            )

        , check
            "formula: TokRParen is a legal FOLLOW token and is not consumed"
            (Right (Formula [Atom O 1, Atom H 1], [TokRParen, TokEOF]))
            ( parseFormula
                [ TokElement O
                , TokElement H
                , TokRParen
                , TokEOF
                ]
            )

        , check
            "formula: a token outside FIRST and FOLLOW is rejected here"
            ( Left
                UnexpectedToken
                    { expected = "element, '(', '+', ')' or end of input"
                    , actual = TokNumber 3
                    }
            )
            (parseFormula [TokElement H, TokNumber 2, TokNumber 3, TokEOF])

        , check
            "formula: missing explicit EOF after a complete atom is rejected"
            ( Left
                UnexpectedEndOfInput
                    { expected = "formula terminator" }
            )
            (parseFormula [TokElement H])

        , check
            "formula: an unclosed group at EOF is rejected"
            (Left UnclosedParenthesis)
            (parseFormula [TokLParen, TokElement O, TokElement H, TokEOF])

          -- parseSpecies: coefficients belong to semantic analysis
        , check
            "species: missing coefficient defaults to 1"
            ( Right
                ( Species
                    { coefficientAST = 1
                    , formulaAST = Formula [Atom H 2]
                    }
                , [TokEOF]
                )
            )
            (parseSpecies [TokElement H, TokNumber 2, TokEOF])

        , check
            "species: coefficient 1 is retained"
            ( Right
                ( Species
                    { coefficientAST = 1
                    , formulaAST = Formula [Atom H 2]
                    }
                , [TokEOF]
                )
            )
            (parseSpecies [TokNumber 1, TokElement H, TokNumber 2, TokEOF])

        , check
            "species: coefficient 0 is preserved for Analyzer validation"
            ( Right
                ( Species
                    { coefficientAST = 0
                    , formulaAST = Formula [Atom H 2]
                    }
                , [TokEOF]
                )
            )
            (parseSpecies [TokNumber 0, TokElement H, TokNumber 2, TokEOF])

        , check
            "species: a coefficient must be followed by a formula"
            ( Left
                UnexpectedToken
                    { expected = "element or ("
                    , actual = TokEOF
                    }
            )
            (parseSpecies [TokNumber 2, TokEOF])

          -- parseReaction: minimum count, separators and final terminator
        , check
            "reaction: exactly two species is the minimum valid reaction"
            ( Right
                ( ReactionAST
                    [ species 1 (Formula [Atom H 2])
                    , species 1 (Formula [Atom O 2])
                    ]
                )
            )
            ( parseReaction
                [ TokElement H
                , TokNumber 2
                , TokPlus
                , TokElement O
                , TokNumber 2
                , TokEOF
                ]
            )

        , check
            "reaction: more than two species is accepted"
            ( Right
                ( ReactionAST
                    [ species 1 (Formula [Atom H 2])
                    , species 1 (Formula [Atom O 2])
                    , species 2 (Formula [Atom H 2, Atom O 1])
                    ]
                )
            )
            ( parseReaction
                [ TokElement H
                , TokNumber 2
                , TokPlus
                , TokElement O
                , TokNumber 2
                , TokPlus
                , TokNumber 2
                , TokElement H
                , TokNumber 2
                , TokElement O
                , TokEOF
                ]
            )

        , check
            "reaction: one species is insufficient"
            (Left InsufficientReactants)
            (parseReaction [TokElement H, TokNumber 2, TokEOF])

        , check
            "reaction: an empty token stream is rejected"
            ( Left
                UnexpectedEndOfInput
                    { expected = "element or (" }
            )
            (parseReaction [])

        , check
            "reaction: plus must be followed by a species"
            ( Left
                UnexpectedToken
                    { expected = "element or ("
                    , actual = TokEOF
                    }
            )
            (parseReaction [TokElement H, TokPlus, TokEOF])

        , check
            "reaction: a closing parenthesis cannot replace the first plus"
            ( Left
                UnexpectedToken
                    { expected = "'+' between reactants"
                    , actual = TokRParen
                    }
            )
            (parseReaction [TokElement H, TokRParen, TokEOF])

        , check
            "reaction: a trailing plus after two species is rejected"
            ( Left
                UnexpectedToken
                    { expected = "element or ("
                    , actual = TokEOF
                    }
            )
            ( parseReaction
                [ TokElement H
                , TokPlus
                , TokElement O
                , TokPlus
                , TokEOF
                ]
            )
        ]

    let passed = length (filter id results)
        total = length results

    putStrLn ("\n" ++ show passed ++ "/" ++ show total ++ " parser tests passed")
    unless (and results) exitFailure

species :: Int -> Formula -> Species
species coefficient formula =
    Species
        { coefficientAST = coefficient
        , formulaAST = formula
        }

check :: (Eq a, Show a) => String -> a -> a -> IO Bool
check name expectedValue actualValue =
    if actualValue == expectedValue
        then do
            putStrLn ("[PASS] " ++ name)
            pure True
        else do
            putStrLn ("[FAIL] " ++ name)
            putStrLn ("  expected: " ++ show expectedValue)
            putStrLn ("  but got:  " ++ show actualValue)
            pure False
