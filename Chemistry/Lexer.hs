module Chemistry.Lexer
    ( lexer
    ) where

import Chemistry.Token
import Data.Char (isSpace)
import Data.List (stripPrefix)

lexer :: String -> Either String [Token]
lexer = scan 1
  where
    scan :: Int -> String -> Either String [Token]

    scan _ [] =
        Right [TokEOF]

    scan column input@(c : rest)
        | isSpace c =
            scan (column + 1) rest

        | c == '+' =
            (TokPlus :) <$> scan (column + 1) rest

        | otherwise =
            case matchToken input of
                Just (token, remaining, lengthConsumed) ->
                    (token :) <$> scan
                        (column + lengthConsumed)
                        remaining

                Nothing ->
                    Left
                        ( "Lexical error at column "
                        ++ show column
                        ++ ": unknown lexeme "
                        ++ show (unknownLexeme input)
                        )


matchToken :: String -> Maybe (Token, String, Int)
matchToken input =
    firstMatch
        [ ("H2SO4", TokH2SO4)
        , ("HCl",   TokHCl)
        , ("Zn",    TokZn)
        , ("Fe",    TokFe)
        , ("Cu",    TokCu)
        ]
  where
    firstMatch [] =
        Nothing

    firstMatch ((lexeme, token) : candidates) =
        case stripPrefix lexeme input of
            Just remaining ->
                Just (token, remaining, length lexeme)

            Nothing ->
                firstMatch candidates


unknownLexeme :: String -> String
unknownLexeme =
    takeWhile isLexemeCharacter
  where
    isLexemeCharacter c =
        not (isSpace c) && c /= '+'