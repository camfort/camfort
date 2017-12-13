module Camfort.Transformation.ArrayIndexSwap (swapIndices) where

import qualified Language.Fortran.AST as F
import Camfort.Analysis
import Camfort.Analysis.Annotations
import Data.Void (Void)

swapIndices ::
     Int    -- src index position
  -> Int    -- targ index position
  -> F.Name -- array name (declared at top-level of the program file)
  -> F.ProgramFile A -> PureAnalysis Void Void (F.ProgramFile A)
swapIndices i j name ps =
  return ps
