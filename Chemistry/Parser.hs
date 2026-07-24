module Chemistry.Parser
    ( parseInput,
    get_Metal_From_Salt
    ) where


import Data.Char (isSpace)

import Chemistry.Types
    ( Metal(..)
    , Acid(..)
    , ReactionInput(..), Salt (ZnCl2, ZnSO4, FeCl2, FeSO4, CuCl2, CuSO4)
    )


parseMetal :: String -> Maybe Metal
parseMetal "Zn" = Just Zn
parseMetal "Fe" = Just Fe
parseMetal "Cu" = Just Cu
parseMetal _    = Nothing


parseAcid :: String -> Maybe Acid
parseAcid "HCl"   = Just HCl
parseAcid "H2SO4" = Just H2SO4
parseAcid _       = Nothing

parseSalt :: String -> Maybe Salt
parseSalt "ZnCl2" = Just ZnCl2
parseSalt "ZnSO4" = Just ZnSO4
parseSalt "FeCl2" = Just FeCl2
parseSalt "FeSO4" = Just FeSO4
parseSalt "CuCl2" = Just CuCl2
parseSalt "CuSO4" = Just CuSO4
parseSalt _ = Nothing

removeSpaces :: String -> String
removeSpaces =
    filter (not . isSpace)

get_Metal_From_Salt :: Salt -> Metal
get_Metal_From_Salt ZnCl2 = Zn
get_Metal_From_Salt ZnSO4 = Zn
get_Metal_From_Salt FeCl2 = Fe
get_Metal_From_Salt FeSO4 = Fe
get_Metal_From_Salt CuCl2 = Cu
get_Metal_From_Salt CuSO4 = Cu

splitOnce :: Char -> String -> Maybe (String, String)
splitOnce separator input =
    case break (== separator) input of
        (left, _ : right) ->
            Just (left, right)
        _ ->
            Nothing

parseInput :: String -> Either String ReactionInput
parseInput rawInput =
    case splitOnce '+' (removeSpaces rawInput) of
        Nothing ->
            Left "输入中缺少反应物分隔符 +"

        Just (left, right) ->
            parseBothOrders left right

parseBothOrders :: String -> String -> Either String ReactionInput
parseBothOrders left right =
    case (parseMetal left, parseAcid right) of
        (Just metal, Just acid) ->
            Right (ReactionInput_Metal_Acid metal acid)

        _ -> case (parseAcid left, parseMetal right) of
            (Just acid, Just metal) ->
                Right (ReactionInput_Metal_Acid metal acid)

            -- 走到这里，说明不是“金属+酸”也不是“酸+金属”。开始尝试“金属+盐”
            _ -> case (parseMetal left, parseSalt right) of
                (Just metal, Just salt) ->
                    Right (ReactionInput_Metal_Salt metal salt)

                -- 走到这里，尝试“盐+金属” (注意左边传入 left，右边传入 right)
                _ -> case (parseSalt left, parseMetal right) of
                    (Just salt, Just metal) ->
                        Right (ReactionInput_Metal_Salt metal salt)

                    -- 所有的组合全失败了，最后保底返回 Left
                    _ -> Left "应输入 Zn、Fe 或 Cu 与 HCl 或 H2SO4或盐"