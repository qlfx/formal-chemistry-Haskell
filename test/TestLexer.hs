module Main where

import Chemistry.Workers.Lexer (lexer)
import Chemistry.Definitions.Token (Token(..))
import Chemistry.Definitions.Types (Element(..))
import Chemistry.Definitions.Error (LexicalError(UnknownLexeme))

tests :: [(String, Either LexicalError [Token])]
tests =
    [ ( "Zn + HCl"
      , Right
            [ TokElement Zn
            , TokPlus
            , TokElement H
            , TokElement Cl
            , TokEOF
            ]
      )

    , ( "HCl + Zn"
      , Right
            [ TokElement H
            , TokElement Cl
            , TokPlus
            , TokElement Zn
            , TokEOF
            ]
      )

    , ( "Fe+H2SO4"
      , Right
            [ TokElement Fe
            , TokPlus
            , TokElement H
            , TokNumber 2
            , TokElement S
            , TokElement O
            , TokNumber 4
            , TokEOF
            ]
      )

    , ( "   Cu   +   HCl   "
      , Right
            [ TokElement Cu
            , TokPlus
            , TokElement H
            , TokElement Cl
            , TokEOF
            ]
      )

    , ( ""
      , Right [TokEOF]
      )

    , ( "Zn HCl"
      , Right
            [ TokElement Zn
            , TokElement H
            , TokElement Cl
            , TokEOF
            ]
      )

    , ( "Ca(OH)2"
      , Left
            (UnknownLexeme
              1 "Ca")
      )

    , ( "Zn + HNO3"
      , Left
            (UnknownLexeme 7 "NO3")
      )
      ,( "(OH)2"
        , Right
    [ TokLParen
    , TokElement O
    , TokElement H
    , TokRParen
    , TokNumber 2
    , TokEOF
    ]
      )
      ,( "Fe(OH)2"
      , Right
    [ TokElement Fe
    , TokLParen
    , TokElement O
    , TokElement H
    , TokRParen
    , TokNumber 2
    , TokEOF
    ]
      )
      ,( "Zn + Fe(OH)2"
    , Right
    [ TokElement Zn
    , TokPlus
    , TokElement Fe
    , TokLParen
    , TokElement O
    , TokElement H
    , TokRParen
    , TokNumber 2
    , TokEOF
    ]
      )
      ,( "H12O6"
    , Right
    [ TokElement H
    , TokNumber 12
    , TokElement O
    , TokNumber 6
    , TokEOF
    ]
    )
    ,(
      "Fe(OH2",
      Right
      [ TokElement Fe
    , TokLParen
    , TokElement O
    , TokElement H
    , TokNumber 2
    , TokEOF
    ]
    )
    ]


main :: IO ()
main =
    mapM_ runTest tests


runTest :: (String, Either LexicalError [Token]) -> IO ()
runTest (source, expected) = do
    let actual = lexer source

    if actual == expected
        then putStrLn ("PASS: " ++ show source)
        else do
            putStrLn ("FAIL: " ++ show source)
            putStrLn ("  expected: " ++ show expected)
            putStrLn ("  actual:   " ++ show actual)