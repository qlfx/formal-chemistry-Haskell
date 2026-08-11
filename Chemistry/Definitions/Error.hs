module Chemistry.Definitions.Error
    (
        CompileError(..)
        ,LexicalError(..),
        SyntaxError(..),
        SemanticError(..)
        ,renderCompileError
    )where

import Chemistry.Definitions.AST
import Chemistry.Definitions.Token
import Chemistry.Definitions.Types
    ( Substance )

data CompileError = 
    LexicalFailure LexicalError
    |SyntaxFailure SyntaxError
    |SemanticFailure SemanticError
    deriving (Eq,Show)

data LexicalError = 
    UnknownLexeme 
    {
        errorOffSet :: Int
        ,errorText :: String
    }
    deriving (Eq,Show)

data SyntaxError = 
    UnexpectedToken 
        {
        expected :: String
        ,actual :: Token
        }
    | UnexpectedEndOfInput
        {
        expected :: String
        }
    | InvalidSubscript Int
    | UnclosedParenthesis
    | InsufficientReactants
    deriving (Eq,Show)

data SemanticError
    = InvalidCoefficient Int
    | UnsupportedFormula Formula
    | UnsupportedReactionCombination [Substance]
    | UnsupportedReactantCount Int
    deriving (Eq, Show)

renderCompileError :: CompileError -> String
renderCompileError compileError = 
    case compileError of
        LexicalFailure lexicalError -> "Lexical Error:" ++ show lexicalError

        SyntaxFailure syntaxError -> "Syntax Error:" ++ show  syntaxError

        SemanticFailure semanticError -> "Semantic Error:"  ++ show semanticError