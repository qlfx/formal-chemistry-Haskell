module Chemistry.Workers.Analyzer (analyzeReaction,analyzeReactionAST,classifyReactionKind) where
import Chemistry.Definitions.Types
import Chemistry.Definitions.AST
import Chemistry.Definitions.Semantic
import Chemistry.Definitions.Error
--SemanticSpecies 中的系数目前被保留，但 ReactionInput 不保存系数，因此这一步只分类反应类型，
--不处理或丢弃配平逻辑；以后配平直接使用 ClassifiedReaction。
analyzeReaction :: ReactionAST -> Either SemanticError ReactionInput 
analyzeReaction reactionAST = do
    classifiedReaction <- analyzeReactionAST reactionAST
    classifyReactionKind classifiedReaction

analyzeReactionAST :: ReactionAST -> Either SemanticError ClassifiedReaction
analyzeReactionAST (ReactionAST speciesList) = do
    semanticSpeciesList <-
        traverseSpecies speciesList
    Right (ClassifiedReaction semanticSpeciesList)

traverseSpecies :: [Species] -> Either SemanticError [SemanticSpecies]
traverseSpecies [] = Right []
traverseSpecies (firstSpecies : rest) = do
    firstSemantic <- analyzeSpecies firstSpecies
    restSemantic <- traverseSpecies rest
    Right (firstSemantic : restSemantic)

analyzeSpecies :: Species -> Either SemanticError SemanticSpecies
analyzeSpecies
    (Species coefficient formula) = do

    validCoefficient <-
        validateCoefficient coefficient

    substance <-
        classifyFormula formula

    Right
        SemanticSpecies
            { coefficientSemantic = validCoefficient
            , formulaSemantic = formula
            , substanceSemantic = substance
            }

validateCoefficient :: Int -> Either SemanticError Int
validateCoefficient coefficient
    | coefficient < 1 =
        Left (InvalidCoefficient coefficient)

    | otherwise =
        Right coefficient

classifyFormula :: Formula -> Either SemanticError Substance
--Metal
classifyFormula
    (Formula [Atom Zn 1]) =
    Right (SubstanceMetal MetalZn)

classifyFormula
    (Formula [Atom Fe 1]) =
    Right (SubstanceMetal MetalFe)

classifyFormula
    (Formula [Atom Cu 1]) =
    Right (SubstanceMetal MetalCu)

--Acid
classifyFormula
    (Formula
        [ Atom H 1
        , Atom Cl 1
        ]) =

    Right (SubstanceAcid AcidHCl)

classifyFormula
    (Formula
        [ Atom H 2
        , Atom S 1
        , Atom O 4
        ]) =
    Right (SubstanceAcid AcidH2SO4)

--Salt
classifyFormula
    (Formula
        [ Atom Zn 1
        , Atom Cl 2
        ]) =
    Right (SubstanceSalt ZnCl2)

classifyFormula
    (Formula
        [ Atom Fe 1
        , Atom Cl 2
        ]) =
    Right (SubstanceSalt FeCl2)

classifyFormula
    (Formula
        [ Atom Zn 1
        , Atom S 1
        , Atom O 4
        ]) =
    Right (SubstanceSalt ZnSO4)

classifyFormula
    (Formula
        [ Atom Fe 1
        , Atom S 1
        , Atom O 4
        ]) =
    Right (SubstanceSalt FeSO4)

classifyFormula
    (Formula
        [ Atom Cu 1
        , Atom Cl 2
        ]) =
    Right (SubstanceSalt CuCl2)

classifyFormula
    (Formula
        [ Atom Cu 1
        , Atom S 1
        , Atom O 4
        ]) =
    Right (SubstanceSalt CuSO4)

classifyFormula formula =
    Left (UnsupportedFormula formula)

classifyReactionKind :: ClassifiedReaction -> Either SemanticError ReactionInput

classifyReactionKind (ClassifiedReaction [firstSpecies, secondSpecies]) =
    classifySpeciesPair firstSpecies secondSpecies

classifyReactionKind
    (ClassifiedReaction speciesList) =
    Left (UnsupportedReactantCount (length speciesList))

classifySpeciesPair :: SemanticSpecies -> SemanticSpecies -> Either SemanticError ReactionInput

classifySpeciesPair
    SemanticSpecies
        { substanceSemantic =
            SubstanceMetal metal
        }
    SemanticSpecies
        { substanceSemantic =
            SubstanceAcid acid
        } =

    Right (ReactionInput_Metal_Acid metal acid)

classifySpeciesPair
    SemanticSpecies
        { substanceSemantic =
            SubstanceAcid acid
        }
    SemanticSpecies
        { substanceSemantic =
            SubstanceMetal metal
        } =

    Right (ReactionInput_Metal_Acid metal acid)

classifySpeciesPair
    SemanticSpecies
        { substanceSemantic =
            SubstanceMetal metal
        }
    SemanticSpecies
        { substanceSemantic =
            SubstanceSalt salt
        } =

    Right (ReactionInput_Metal_Salt metal salt)

classifySpeciesPair
    SemanticSpecies
        { substanceSemantic =
            SubstanceSalt salt
        }
    SemanticSpecies
        { substanceSemantic =
            SubstanceMetal metal
        } =

    Right (ReactionInput_Metal_Salt metal salt)

classifySpeciesPair
    firstSpecies
    secondSpecies =

    Left (UnsupportedReactionCombination
            [ substanceSemantic firstSpecies
            , substanceSemantic secondSpecies
            ])