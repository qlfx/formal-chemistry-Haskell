module Main where

import Chemistry.Lexer (lexer)
import Chemistry.Token (Token(..))

tests :: [(String, Either String [Token])]
tests =
    [ ( "Zn + HCl"
      , Right [TokZn, TokPlus, TokHCl, TokEOF]
      )

    , ( "HCl + Zn"
      , Right [TokHCl, TokPlus, TokZn, TokEOF]
      )

    , ( "Fe+H2SO4"
      , Right [TokFe, TokPlus, TokH2SO4, TokEOF]
      )

    , ( "   Cu   +   HCl   "
      , Right [TokCu, TokPlus, TokHCl, TokEOF]
      )

    , ( ""
      , Right [TokEOF]
      )

    , ( "Zn HCl"
      , Right [TokZn, TokHCl, TokEOF]
      )

    , ( "Mg + HCl"
      , Left
          "Lexical error at column 1: unknown lexeme \"Mg\""
      )

    , ( "Zn + HNO3"
      , Left
          "Lexical error at column 6: unknown lexeme \"HNO3\""
      )
    ]


main :: IO ()
main =
    mapM_ runTest tests


runTest :: (String, Either String [Token]) -> IO ()
runTest (source, expected) = do
    let actual = lexer source

    if actual == expected
        then putStrLn ("PASS: " ++ show source)
        else do
            putStrLn ("FAIL: " ++ show source)
            putStrLn ("  expected: " ++ show expected)
            putStrLn ("  actual:   " ++ show actual)