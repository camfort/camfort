{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE LambdaCase #-}

module Camfort.Specification.Hoare where

import Control.Monad.Except

import qualified Language.Fortran.Analysis          as FA
import qualified Language.Fortran.Analysis.BBlocks  as FAB
import qualified Language.Fortran.Analysis.Renaming as FAR
import qualified Language.Fortran.AST               as F
import qualified Language.Fortran.Util.Position     as F

import           Language.While.Hoare
import qualified Language.While.Syntax              as W

import           Camfort.Helpers
import Camfort.Specification.Hoare.Parser

-- type HoareAnn = Maybe (WhileProp String)


-- data TranslateError a
--   = TranslateError
--   | UnsupportedBlock F.SrcSpan (F.Block a)
--   | UnsupportedStatement F.SrcSpan (F.Statement a)


-- seqCommands :: [W.Command l a] -> W.Command l a
-- seqCommands [c] = c
-- seqCommands cs = foldr1 W.CSeq cs


-- fortranToWhile :: [F.Block a] -> Either (TranslateError a) (W.Command String a)
-- fortranToWhile = fmap seqCommands . traverse translateBlock
--   where
--     translateBlock = \case
--       F.BlStatement ann span label st -> translateStatement st
--       F.BlIf ann span label name conds bodies endLabel -> undefined
--       b@(F.BlCase ann span _ _ _ _ _ _) -> throwError (UnsupportedBlock span b)
--       b@(F.BlDo ann span _ _ _ _ _ _) -> throwError (UnsupportedBlock span b)
--       b@(F.BlDoWhile ann span _ _ _ _ _) -> throwError (UnsupportedBlock span b)
--       b@(F.BlInterface ann span _ _ _) -> throwError (UnsupportedBlock span b)
--       b@(F.BlComment ann span _) -> throwError (UnsupportedBlock span b)

--     translateStatement = undefined

-- check :: Filename -> F.ProgramFile HoareAnn -> String
-- check = undefined


--   --   BlStatement a SrcSpan
--   --               (Maybe (Expression a))       -- Label
--   --               (Statement a)                -- Statement

--   -- | BlIf        a SrcSpan
--   --               (Maybe (Expression a))       -- Label
--   --               (Maybe String)               -- Construct name
--   --               [ Maybe (Expression a) ]     -- Conditions
--   --               [ [ Block a ] ]              -- Bodies
--   --               (Maybe (Expression a))       -- Label to END IF

--   -- | BlCase      a SrcSpan
--   --               (Maybe (Expression a))       -- Label
--   --               (Maybe String)               -- Construct name
--   --               (Expression a)               -- Scrutinee
--   --               [ Maybe (AList Index a) ]    -- Case ranges
--   --               [ [ Block a ] ]              -- Bodies
--   --               (Maybe (Expression a))       -- Label to END SELECT

--   -- | BlDo        a SrcSpan
--   --               (Maybe (Expression a))       -- Label
--   --               (Maybe String)               -- Construct name
--   --               (Maybe (Expression a))       -- Target label
--   --               (Maybe (DoSpecification a))  -- Do Specification
--   --               [ Block a ]                  -- Body
--   --               (Maybe (Expression a))       -- Label to END DO

--   -- | BlDoWhile   a SrcSpan
--   --               (Maybe (Expression a))       -- Label
--   --               (Maybe String)               -- Construct name
--   --               (Expression a)               -- Condition
--   --               [ Block a ]                  -- Body
--   --               (Maybe (Expression a))       -- Label to END DO

--   -- | BlInterface a SrcSpan
--   --               (Maybe (Expression a))       -- label
--   --               [ ProgramUnit a ]            -- Routine decls. in the interface
--   --               [ Block a ]                  -- Module procedures

--   -- | BlComment a SrcSpan (Comment a)
