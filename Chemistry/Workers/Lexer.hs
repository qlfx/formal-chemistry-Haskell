module Chemistry.Workers.Lexer
    ( lexer
    ) where

import Chemistry.Definitions.Token
import Chemistry.Definitions.Types(Element(..))
import Data.Char (isSpace,isDigit)
import Data.List (stripPrefix)
import Chemistry.Definitions.Error

lexer :: String -> Either LexicalError [Token]
lexer = scan 1
  where
    scan :: Int -> String -> Either LexicalError [Token]

    scan _ [] =
        Right [TokEOF]

    scan column input@(c : rest)
        | isSpace c = scan (column + 1) rest

        | c == '+' = (TokPlus :) <$> scan (column + 1) rest


        | c == '(' = (TokLParen :) <$> scan (column + 1) rest

        | c == ')' = (TokRParen :) <$> scan (column + 1) rest

        | isDigit c =
            let (digits, remaining) =
                    span isDigit input

                value =
                    read digits
            in
                (TokNumber value :)
                    <$> scan
                        (column + length digits)
                        remaining

        | otherwise =
            case matchToken input of
                Just (token, remaining, lengthConsumed) ->
                    (token :) <$> scan
                        (column + lengthConsumed)
                        remaining

                Nothing ->
                    Left
                        (UnknownLexeme
                            column
                            (unknownLexeme input))

matchToken :: String -> Maybe (Token, String, Int)
matchToken input =
    firstMatch
        [ ("Cl", TokElement Cl)
        , ("Zn", TokElement Zn)
        , ("Fe", TokElement Fe)
        , ("Cu", TokElement Cu)
        , ("H",  TokElement H)
        , ("O",  TokElement O)
        , ("S",  TokElement S)
        ]
  where
    firstMatch [] =
        Nothing

    firstMatch ((lexeme, token) : candidates) =
        case stripPrefix lexeme input of
            Just remaining ->
                Just
                    ( token
                    , remaining
                    , length lexeme
                    )

            Nothing ->
                firstMatch candidates

unknownLexeme :: String -> String
unknownLexeme =
    takeWhile isLexemeCharacter
  where
    isLexemeCharacter c =
        not (isSpace c)
        && c /= '+'
        && c /= '('
        && c /= ')'