module Chemistry.Analyzer (analyzeReaction) where
import qualified Chemistry.AST as AST
import qualified Chemistry.Types as Domain
-- import Control.Arrow (ArrowChoice(right))
import Chemistry.Types (ReactionInput)

data SubstanceClass
    = ClassifiedMetal Domain.Metal
    | ClassifiedAcid Domain.Acid
    | ClassifiedSalt Domain.Salt
    deriving(Eq,Show)

analyzeReaction
    :: AST.ReactionAST
    -> Either String Domain.ReactionInput

analyzeReaction
    (AST.ReactionAST [left, right]) =
        analyzeBothOrders
            (classifySpecies left)
            (classifySpecies right)

analyzeReaction
    (AST.ReactionAST speciesList) =
        Left
            ( "当前求值器只支持两个反应物，实际得到 "
                ++ show (length speciesList)
                ++ " 个"
            )

analyzeBothOrders :: Either String SubstanceClass -> Either String SubstanceClass -> Either String ReactionInput
analyzeBothOrders leftResult rightResult = do
    left  <- leftResult
    right <- rightResult

    case (left, right) of
        ( ClassifiedMetal metal
            , ClassifiedAcid acid
            ) ->
                Right
                    (Domain.ReactionInput_Metal_Acid
                        metal
                        acid)

        ( ClassifiedAcid acid
            , ClassifiedMetal metal
            ) ->
                Right
                    (Domain.ReactionInput_Metal_Acid
                        metal
                        acid)

        ( ClassifiedMetal metal
            , ClassifiedSalt salt
            ) ->
                Right
                    (Domain.ReactionInput_Metal_Salt
                        metal
                        salt)

        ( ClassifiedSalt salt
            , ClassifiedMetal metal
            ) ->
                Right
                    (Domain.ReactionInput_Metal_Salt
                        metal
                        salt)

        _ ->
            Left
                "当前只支持金属+酸或金属+盐"

classifySpecies :: AST.Species -> Either String SubstanceClass
classifySpecies species = 
    case classifyFormula 
            (AST.formulaAST species) of
                Just result -> 
                    Right result

                Nothing ->
                    Left
                        ("无法识别化学式AST：" ++ show (AST.formulaAST species))

classifyFormula :: AST.Formula -> Maybe SubstanceClass
classifyFormula formulaValue  
    | formulaValue == znFormula =
    Just
            (ClassifiedMetal Domain.Zn)

    | formulaValue == feFormula =
        Just
            (ClassifiedMetal Domain.Fe)

    | formulaValue == cuFormula =
        Just
            (ClassifiedMetal Domain.Cu)

    | formulaValue == hclFormula =
        Just
            (ClassifiedAcid Domain.HCl)

    | formulaValue == h2so4Formula =
        Just
            (ClassifiedAcid Domain.H2SO4)

    | formulaValue == zncl2Formula =
        Just
            (ClassifiedSalt Domain.ZnCl2)

    | formulaValue == znso4Formula =
        Just
            (ClassifiedSalt Domain.ZnSO4)

    | formulaValue == fecl2Formula =
        Just
            (ClassifiedSalt Domain.FeCl2)

    | formulaValue == feso4Formula =
        Just
            (ClassifiedSalt Domain.FeSO4)

    | formulaValue == cucl2Formula =
        Just
            (ClassifiedSalt Domain.CuCl2)

    | formulaValue == cuso4Formula =
        Just
            (ClassifiedSalt Domain.CuSO4)

    | otherwise =
        Nothing

znFormula :: AST.Formula
znFormula =
    AST.Formula
        [AST.Atom AST.Zn 1]


feFormula :: AST.Formula
feFormula =
    AST.Formula
        [AST.Atom AST.Fe 1]


cuFormula :: AST.Formula
cuFormula =
    AST.Formula
        [AST.Atom AST.Cu 1]


hclFormula :: AST.Formula
hclFormula =
    AST.Formula
        [ AST.Atom AST.H 1
        , AST.Atom AST.Cl 1
        ]


h2so4Formula :: AST.Formula
h2so4Formula =
    AST.Formula
        [ AST.Atom AST.H 2
        , AST.Atom AST.S 1
        , AST.Atom AST.O 4
        ]


zncl2Formula :: AST.Formula
zncl2Formula =
    AST.Formula
        [ AST.Atom AST.Zn 1
        , AST.Atom AST.Cl 2
        ]


znso4Formula :: AST.Formula
znso4Formula =
    AST.Formula
        [ AST.Atom AST.Zn 1
        , AST.Atom AST.S 1
        , AST.Atom AST.O 4
        ]


fecl2Formula :: AST.Formula
fecl2Formula =
    AST.Formula
        [ AST.Atom AST.Fe 1
        , AST.Atom AST.Cl 2
        ]


feso4Formula :: AST.Formula
feso4Formula =
    AST.Formula
        [ AST.Atom AST.Fe 1
        , AST.Atom AST.S 1
        , AST.Atom AST.O 4
        ]


cucl2Formula :: AST.Formula
cucl2Formula =
    AST.Formula
        [ AST.Atom AST.Cu 1
        , AST.Atom AST.Cl 2
        ]


cuso4Formula :: AST.Formula
cuso4Formula =
    AST.Formula
        [ AST.Atom AST.Cu 1
        , AST.Atom AST.S 1
        , AST.Atom AST.O 4
        ]