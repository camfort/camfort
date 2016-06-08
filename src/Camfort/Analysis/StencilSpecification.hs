{-
   Copyright 2016, Dominic Orchard, Andrew Rice, Mistral Contrastin, Matthew Danish

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Camfort.Analysis.StencilSpecification where

import Language.Fortran hiding (Spec)

import Data.Data
import Data.Generics.Uniplate.Operations
import Control.Arrow
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer hiding (Product)

import Camfort.Analysis.StencilSpecification.Check
import qualified Camfort.Analysis.StencilSpecification.Grammar as Gram
import Camfort.Analysis.StencilSpecification.Model
import Camfort.Analysis.StencilSpecification.Inference
import Camfort.Analysis.StencilSpecification.Synthesis
import Camfort.Analysis.StencilSpecification.Syntax
import Camfort.Analysis.Loops (collect)
import Camfort.Analysis.Annotations
import Camfort.Analysis.CommentAnnotator
import Camfort.Extensions.UnitsForpar (parameterise)
import Camfort.Helpers.Vec
-- These two are redefined here for ForPar ASTs
import Camfort.Helpers hiding (lineCol, spanLineCol)
import Camfort.Output

import qualified Language.Fortran.AST as F
import qualified Language.Fortran.Analysis as FA
import qualified Language.Fortran.Analysis.Types as FAT
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.Analysis.BBlocks as FAB
import qualified Language.Fortran.Analysis.DataFlow as FAD
import qualified Language.Fortran.Util.Position as FU

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Tuple (swap)
import Data.Ord

import Debug.Trace

--------------------------------------------------
-- For the purposes of development, a representative example is given by running (in ghci):
--      stencilsInf "samples/stencils/one.f" [] () ()

infer :: F.ProgramFile Annotation -> String
infer = concatMap (formatSpec M.empty) . FAR.underRenaming (infer' . FAB.analyseBBlocks)
infer' pf@(F.ProgramFile cm_pus _) = concatMap perPU cm_pus
  where
    perPU (_, pu) = runInferer cycs2 (F.getName pu) tenv (descendBiM perBlock pu)
    bm    = FAD.genBlockMap pf     -- get map of AST-Block-ID ==> corresponding AST-Block
    bbm   = FAB.genBBlockMap pf    -- get map of program unit ==> basic block graph
    sgr   = FAB.genSuperBBGr bbm   -- stitch all of the graphs together into a 'supergraph'
    gr    = FAB.superBBGrGraph sgr -- extract the supergraph itself
    dm    = FAD.genDefMap bm       -- get map of variable name ==> { defining AST-Block-IDs }
    rd    = FAD.reachingDefinitions dm gr   -- perform reaching definitions analysis
    flTo  = FAD.genFlowsToGraph bm dm gr rd -- create graph of definition "flows"
    -- VarFlowsToMap: A -> { B, C } indicates that A contributes to B, C.
    flMap = FAD.genVarFlowsToMap dm flTo -- create VarFlowsToMap
    -- find 2-cycles: A -> B -> A
    cycs2 = [ (n, m) | (n, ns) <- M.toList flMap
                     , m       <- S.toList ns
                     , ms      <- maybeToList $ M.lookup m flMap
                     , n `S.member` ms && n /= m ]
    beMap = FAD.genBackEdgeMap (FAD.dominators gr) gr -- identify every loop by its back-edge
    ivMap = FAD.genInductionVarMap beMap gr -- get [basic] induction variables for every loop
    tenv  = FAT.inferTypes pf

-- | Return list of variable names that flow into themselves via a 2-cycle
findVarFlowCycles :: Data a => F.ProgramFile a -> [(F.Name, F.Name)]
findVarFlowCycles = FAR.underRenaming (findVarFlowCycles' . FAB.analyseBBlocks)
findVarFlowCycles' pf = cycs2
  where
    bm    = FAD.genBlockMap pf     -- get map of AST-Block-ID ==> corresponding AST-Block
    bbm   = FAB.genBBlockMap pf    -- get map of program unit ==> basic block graph
    sgr   = FAB.genSuperBBGr bbm   -- stitch all of the graphs together into a 'supergraph'
    gr    = FAB.superBBGrGraph sgr -- extract the supergraph itself
    dm    = FAD.genDefMap bm       -- get map of variable name ==> { defining AST-Block-IDs }
    rd    = FAD.reachingDefinitions dm gr   -- perform reaching definitions analysis
    flTo  = FAD.genFlowsToGraph bm dm gr rd -- create graph of definition "flows"
    -- VarFlowsToMap: A -> { B, C } indicates that A contributes to B, C.
    flMap = FAD.genVarFlowsToMap dm flTo -- create VarFlowsToMap
    -- find 2-cycles: A -> B -> A
    cycs2 = [ (n, m) | (n, ns) <- M.toList flMap
                     , m       <- S.toList ns
                     , ms      <- maybeToList $ M.lookup m flMap
                     , n `S.member` ms && n /= m ]

instance ASTEmbeddable Annotation Gram.Specification where
  annotateWithAST ann ast = ann { stencilSpec = Just $ Left ast }

instance Linkable Annotation where
  link ann (b@(F.BlDo {})) =
      ann { stencilBlock = Just b }
  link ann (b@(F.BlStatement _ _ _ (F.StExpressionAssign {}))) =
      ann { stencilBlock = Just b }
  link ann b = ann


check :: F.ProgramFile Annotation -> String
check pf = intercalate "\n" . snd . runWriter $ do
   pf' <- annotateComments Gram.specParser pf
   tell . pprint . snd . fst $ runState (runWriterT $ descendBiM perBlockCheck pf') ([], [])
     where pprint =  map (\(span, spec) -> show span ++ "\t" ++ spec)

-- If the annotation contains an unconverted stencil specification syntax tree
-- then convert it and return an updated annotation containing the AST
parseCommentToAST :: Annotation -> FU.SrcSpan ->
  WriterT [(FU.SrcSpan, String)] (State (RegionEnv, [Variable])) Annotation
parseCommentToAST ann span =
  case stencilSpec ann of
    Just (Left stencilComment) -> do
         (regionEnv, _) <- get
         let ?renv = regionEnv
          in case synToAst stencilComment of
               Left err   -> error $ show span ++ ": " ++ err
               Right ast  -> return $ ann { stencilSpec = Just (Right ast) }
    _ -> return ann

-- If the annotation contains an encapsulated region environment, extract it
-- and add it to current region environment in scope
updateRegionEnv :: Annotation -> WriterT [(FU.SrcSpan, String)]
        (State (RegionEnv, [Variable])) ()
updateRegionEnv ann =
  case stencilSpec ann of
    Just (Right (Left regionEnv)) -> modify $ ((++) regionEnv) *** id
    _                             -> return ()

-- For a list of names -> specs, compare the model for each
-- against any matching names in the spec env (second param)
compareModel :: [([F.Name], Specification)] -> SpecDecls -> Bool
compareModel [] _ = True
compareModel ((names, spec) : ss) env =
  foldr (\n r -> compareModel' n spec env && r) True names
  && compareModel ss env
   where compareModel' :: F.Name -> Specification -> SpecDecls -> Bool
         compareModel' name spec1 senv =
          case lookupSpecDecls senv name of
            Just spec2 -> let d1 = dimensionality spec1
                              d2 = dimensionality spec2
                          in let ?dimensionality = d1 `max` d2
                             in mkModel spec1 == mkModel spec2
            Nothing    -> True

perBlockCheck :: F.Block Annotation
   -> WriterT [(FU.SrcSpan, String)]
        (State (RegionEnv, [Variable])) (F.Block Annotation)

perBlockCheck b@(F.BlComment ann span _) = do
  ann' <- parseCommentToAST ann span
  updateRegionEnv ann'
  let b' = F.setAnnotation ann' b
  case (stencilSpec ann', stencilBlock ann') of
    -- Comment contains a specification and an associated block
    (Just (Right (Right specDecls)), Just block) ->
     case block of
      s@(F.BlStatement ann span _ (F.StExpressionAssign _ _ _ rhs)) -> do
        -- Get array indexing (on the RHS)
        let rhsExprs = universeBi rhs :: [F.Expression Annotation]
        let arrayAccesses = collect [
              (v, e) | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) subs <- rhsExprs
                     , let e = F.aStrip subs
                     , not (null e)]
        -- Create list of relative indices
        (_, ivs) <- get
        let analysis = groupKeyBy
                     . M.toList
                     . M.mapMaybe (ixCollectionToSpec ivs) $ arrayAccesses
        -- Model and compare the current and specified stencil specs
        if compareModel analysis specDecls
           -- Not well-specified
         then tell [ (span, "Correct.") ]
         else tell [ (span, "Not well specified:\n\t\t  expecting: "
                         ++ pprintSpecDecls specDecls
                         ++ "\t\t  actual:    " ++ pprintSpecDecls analysis) ]
        return $ b'
      _ -> return $ b'

      (F.BlDo ann span _ mDoSpec body) -> do
        -- Stub, collect stencils inside 'do' block
        return $ b'
      _ -> return $ b'
    _ -> return b'

perBlockCheck b@(F.BlDo ann span _ mDoSpec body) = do
   let localIvs = getInductionVar mDoSpec
   -- introduce any induction variables into the induction variable state
   modify $ id *** union localIvs
   -- descend into the body of the do-statement
   mapM_ (descendBiM perBlockCheck) body
   -- Remove any induction variable from the state
   modify $ id *** (\\ localIvs)
   return b

perBlockCheck b = do
  updateRegionEnv . F.getAnnotation $ b
  -- Go inside child blocks
  mapM_ (descendBiM perBlockCheck) $ children b
  return b



--------------------------------------------------

type LogLine = (FU.SrcSpan, [([Variable], Specification)])
formatSpec :: FAR.NameMap -> LogLine -> String
formatSpec nm (span, []) = ""
formatSpec nm (span, specs) = loc ++ " \t" ++ (commaSep . nub . map doSpec $ specs) ++ "\n"
  where
    loc                      = show (spanLineCol span)
    commaSep                 = intercalate ", "
    doSpec (arrayVar, spec)  = show (fixSpec spec) ++ " :: " ++ commaSep (map realName arrayVar)
    realName v               = v `fromMaybe` (v `M.lookup` nm)
    fixSpec (Specification (Right (Dependency vs b))) =
        Specification (Right (Dependency (map realName vs) b))
    fixSpec s                = s

--------------------------------------------------

-- The inferer works within this monad
type Inferer = WriterT [LogLine] (ReaderT (Cycles, F.ProgramUnitName, TypeEnv A) (State [Variable]))
type Cycles = [(F.Name, F.Name)]
runInferer :: Cycles -> F.ProgramUnitName -> TypeEnv A -> Inferer a -> [LogLine]
runInferer cycles puName tenv =
  flip evalState [] . flip runReaderT (cycles, puName, tenv) . execWriterT

--------------------------------------------------

getInductionVar (Just (F.DoSpecification _ _ (
                        F.StExpressionAssign _ _ (
                          F.ExpValue _ _ (F.ValVariable v)) _) _ _)) = [v]
getInductionVar _ = []

-- Idea: allow the behaviour of the whole-program spec inference to
-- be switched between two modes:
--  * infer for all assigns to affine induction subscripted arrays inside loops
--  * infer for all do-loops with affine induction subscripte reads
-- Considerations: need to be able to call inference from checking part too
-- perDoBlock should be easy

isStencilDo :: F.Block (FA.Analysis A) -> Bool
isStencilDo b@(F.BlDo _ span _ mDoSpec body) = hasAffineSubscriptsOnIVar b
isStencilDo _                                = False

hasAffineSubscriptsOnIVar :: forall a . Data a => F.Block a -> Bool
hasAffineSubscriptsOnIVar b@(F.BlDo _ _ _ mDoSpec body) =
  case getInductionVar mDoSpec of
    [] -> False
    [ivar] ->
       and [ all (\sub -> sub `isAffineOnVar` ivar) subs' |
              F.ExpSubscript _ _ _ subs <- universeBi body :: [F.Expression a]
              , let subs' = F.aStrip subs
              , not (null subs') ]

-- Only infer specification on those classified to be stencil do-blocks
-- by the above predicate
perDoBlock :: F.Block (FA.Analysis A) -> Inferer (F.Block (FA.Analysis A))
perDoBlock b@(F.BlDo _ span _ mDoSpec body) | isStencilDo b = do
  let localIvs = getInductionVar mDoSpec

  -- ** INSERT ANALYSIS HERE
  
  -- introduce any induction variables into the induction variable state
  modify $ union localIvs
  -- descend into the body of the do-statement
  mapM_ (descendBiM perDoBlock) body
  -- Remove any induction variable from the state
  modify $ (\\ localIvs)
  return b

perDoBlock b = return b


perBlock :: F.Block (FA.Analysis A) -> Inferer (F.Block (FA.Analysis A))
{-
perBlock b@(F.BlStatement _ span _ (F.StExpressionAssign _ _ lhs rhs)) = do
 inductionVars <- get
 case lhs of
   F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) subs
     -> case (mapM_ (expToOffset inductionVars) subs)
          -- all subscripts are affine induction exrepssions
          Just () ->
          -- subscript are non-affine
          Nothing -> return ()
   _ -> -- Expression is not an array subscript
-}

perBlock b@(F.BlStatement _ span _ (F.StExpressionAssign _ _ _ rhs)) = do
  (_, puName, tenv) <- ask
  -- Get array indexing (on the RHS)
  let rhsExprs = universeBi rhs :: [F.Expression (FA.Analysis A)]
  let arrayAccesses = collect [
          (v, e) | F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) subs <- rhsExprs
                 , let e = F.aStrip subs
                 , not (null e)
        ]

  -- Create specification information
  ivs <- get
  let specs = groupKeyBy . M.toList . M.mapMaybe (ixCollectionToSpec ivs) $ arrayAccesses
  -- add to report
  tell [ (span, specs) ]
  return b

perBlock b@(F.BlDo _ span _ mDoSpec body) = do
  let localIvs = getInductionVar mDoSpec
  -- introduce any induction variables into the induction variable state
  modify $ union localIvs

  (cycles, _, _) <- ask

  let lexps = FA.lhsExprs =<< body

  let getTimeSpec e = do
        lhsV <- case e of
          F.ExpValue _ _ (F.ValVariable lhsV) -> Just lhsV
          F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable lhsV)) _ -> Just lhsV
          _ -> Nothing
        v'   <- lookup lhsV cycles
        -- TODO: update with mutual info
        return (lhsV, Specification $ Right $ Dependency [v'] False)

  let tempSpecs = groupKeyBy $ foldl' (\ ts -> maybe ts (:ts) . getTimeSpec) [] lexps

  tell [ (span, tempSpecs) ]
  -- descend into the body of the do-statement
  mapM_ (descendBiM perBlock) body
  -- (we don't need to worry about scope, thanks to renaming)

  -- Remove any induction variable from the state
  modify $ (\\ localIvs)
  return b

perBlock b = do
  -- Go inside child blocks
  mapM_ (descendBiM perBlock) $ children b
  return b

-- Penelope's first code, 20/03/2016.
-- iii././//////////////////////. mvnmmmmmmmmmu

{- *** 2 . Operations on specs, and conversion from indexing expressions -}

-- padZeros makes this rectilinear
padZeros :: [[Int]] -> [[Int]]
padZeros ixss = let m = maximum (map length ixss)
                in map (\ixs -> ixs ++ replicate (m - length ixs) 0) ixss

-- Convert list of indexing expressions to a spec
ixCollectionToSpec :: [ Variable ] -> [ [ F.Index a ] ] -> Maybe Specification
ixCollectionToSpec ivs ixs =
  if isEmpty exactSpec
  then Nothing else Just exactSpec
    where
     exactSpec = inferFromIndices
               . fromLists
               . padZeros
               . map (toListsOfRelativeIndices ivs) $ ixs

toListsOfRelativeIndices :: [ Variable ] -> [ F.Index a ] -> [ Int ]
toListsOfRelativeIndices ivs = map (maybe 0 id . (ixToOffset ivs))

-- Convert indexing expressions that are translations
-- intto their translation offset:
-- e.g., for the expression a(i+1,j-1) then this function gets
-- passed expr = i + 1   (returning +1) and expr = j - 1 (returning -1)
ixToOffset :: [Variable] -> F.Index a -> Maybe Int
ixToOffset ivs (F.IxSingle _ _ _ exp) = expToOffset ivs exp
ixToOffset _ _ = Nothing -- If the indexing expression is a range

isAffineOnVar :: F.Index a -> Variable -> Bool
isAffineOnVar exp v = ixToOffset [v] exp /= Nothing

expToOffset :: [Variable] -> F.Expression a -> Maybe Int
expToOffset ivs (F.ExpValue _ _ (F.ValVariable v))
  | v `elem` ivs = Just 0
  | otherwise    = Just absoluteRep
expToOffset ivs (F.ExpBinary _ _ F.Addition
                                 (F.ExpValue _ _ (F.ValVariable v))
                                 (F.ExpValue _ _ (F.ValInteger offs)))
    | v `elem` ivs = Just $ read offs
expToOffset ivs (F.ExpBinary _ _ F.Addition
                                 (F.ExpValue _ _ (F.ValInteger offs))
                                 (F.ExpValue _ _ (F.ValVariable v)))
    | v `elem` ivs = Just $ read offs
expToOffset ivs (F.ExpBinary _ _ F.Subtraction
                                 (F.ExpValue _ _ (F.ValVariable v))
                                 (F.ExpValue _ _ (F.ValInteger offs)))
   | v `elem` ivs = Just $ if x < 0 then abs x else (- x)
                     where x = read offs
expToOffset ivs _ = Just absoluteRep

--------------------------------------------------

lineCol :: FU.Position -> (Int, Int)
lineCol p  = (fromIntegral $ FU.posLine p, fromIntegral $ FU.posColumn p)

spanLineCol :: FU.SrcSpan -> ((Int, Int), (Int, Int))
spanLineCol (FU.SrcSpan l u) = (lineCol l, lineCol u)

groupKeyBy :: Eq b => [(a, b)] -> [([a], b)]
groupKeyBy = groupKeyBy' . map (\ (k, v) -> ([k], v))

groupKeyBy' []                         = []
groupKeyBy' [(ks, v)]                  = [(ks, v)]
groupKeyBy' ((ks1, v1):((ks2, v2):xs))
  | v1 == v2                           = groupKeyBy' ((ks1 ++ ks2, v1) : xs)
  | otherwise                          = (ks1, v1) : groupKeyBy' ((ks2, v2) : xs)

-- Although type analysis isn't necessary anymore (Forpar does it
-- internally) I'm going to leave this infrastructure in-place in case
-- it might be useful later.
type TypeEnv a = M.Map FAT.TypeScope (M.Map String FA.IDType)
isArrayType :: TypeEnv A -> F.ProgramUnitName -> String -> Bool
isArrayType tenv name v = fromMaybe False $ do
  tmap <- M.lookup (FAT.Local name) tenv `mplus` M.lookup FAT.Global tenv
  idty <- M.lookup v tmap
  cty  <- FA.idCType idty
  return $ cty == FA.CTArray

-- Local variables:
-- mode: haskell
-- haskell-program-name: "cabal repl"
-- End:
