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

{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import Data.Foldable (find)

import System.Console.GetOpt
import System.Environment

import Camfort.Helpers hiding ((<>))
import Camfort.Input (defaultValue)
import Camfort.Functionality
import Camfort.Specification.Stencils (InferMode)
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (pack, unpack, split)

import Options.Applicative


-- | Commands supported by CamFort.
data Command = CmdCount ReadOptions
             | CmdAST ReadOptions
             | CmdStencilsCheck ReadOptions
             | CmdStencilsInfer StencilsOptions
             | CmdStencilsSynth StencilsSynthOptions
             | CmdUnitsSuggest UnitsWriteOptions
             | CmdUnitsCheck UnitsOptions
             | CmdUnitsInfer UnitsOptions
             | CmdUnitsSynth UnitsSynthOptions
             | CmdUnitsCompile UnitsWriteOptions
             | CmdRefactCommon RefactOptions
             | CmdRefactDead RefactOptions
             | CmdRefactEquivalence RefactOptions
             | CmdRefactDatatype RefactOptions
             | CmdTopVersion


-- | Options for reading files.
data ReadOptions = ReadOptions
  { inputSource :: String
  , exclude     :: Maybe [String]
  }


-- | Options for writing to files.
--
-- User can choose to either specify which file to write, or have
-- the input files be written over.
data WriteOptions = WriteFile { outputFile :: String }
                  | WriteInplace


-- | Options used by stencil commands.
data StencilsOptions = StencilsOptions
  { soReadOptions  :: ReadOptions
  , soWriteOptions :: WriteOptions
  , soInferMode    :: InferMode
  }


-- | Options used by all unit commands.
data UnitsOptions = UnitsOptions
  { uoReadOptions :: ReadOptions
  , literals      :: LiteralsOpt
  , debug         :: Bool
  , includeDir    :: Maybe String
  }


data UnitsWriteOptions = UnitsWriteOptions
  { uwoUnitsOptions :: UnitsOptions
  , uwoWriteOptions :: WriteOptions
  }


newtype AnnotationOptions =
  AnnotationOptions { annotationType :: AnnotationType }


data UnitsSynthOptions = UnitsSynthOptions
  { usoUnitsWriteOptions :: UnitsWriteOptions
  , usoAnnotationOptions :: AnnotationOptions
  }


data StencilsSynthOptions = StencilsSynthOptions
  { ssoStencilsOptions    :: StencilsOptions
  , ssoAnnotationOptions  :: AnnotationOptions
  }


-- | Options used by refactoring commands.
data RefactOptions = RefactOptions
  { rfoReadOptions  :: ReadOptions
  , rfoWriteOptions :: WriteOptions
  }


-- | Parser for an argument representing an individual file or directory.
fileArgument :: Mod ArgumentFields String -> Parser String
fileArgument m = strArgument (metavar "FILENAME" <> action "file" <> m)


-- | Parser for file options with multiple files specified
-- | as a comma-separated list.
multiFileOption :: Mod OptionFields [String] -> Parser [String]
multiFileOption m = option (list str)
                    (metavar "FILE..." <> action "file" <> m)
  where list :: ReadM String -> ReadM [String]
        list = fmap (splitBy ',')
        splitBy c [] = []
        splitBy c xs = case break (==c) xs of
                         (xs', [])     -> [xs']
                         (xs', _:cs) -> xs' : splitBy c cs


excludeOption :: Parser (Maybe [String])
excludeOption = optional $ multiFileOption $
     long "exclude"
  <> short 'e'
  <> help "files to exclude (comma separated list, no spaces)"


-- | Parse options for 'ReadOptions'.
readOptions :: Parser ReadOptions
readOptions = fmap ReadOptions
  (fileArgument $ help "input file")
  <*> excludeOption


-- | User must specify either an ouput file, or say that the file
-- | should be rewritten in place.
writeOptions :: Parser WriteOptions
writeOptions = (fmap WriteFile . fileArgument $
                 help "file to write output to")
               <|> (pure WriteInplace <* flag' ()
                     (   long "inplace"
                      <> help "write in place (replaces input files)"))


stencilsOptions :: Parser StencilsOptions
stencilsOptions = fmap StencilsOptions
  readOptions <*> writeOptions <*> inferModeOption
  where
    inferModeOption = fmap (fromMaybe defaultValue)
                      . optional . option readInferOption $
      (    metavar "INFER-MODE"
        <> completeWith ["Assign", "Do", "Both"]
        <> long "stencil-inference-mode"
        <> short 'm'
        <> help "stencil specification inference mode. ID = Do, Assign, or Both")
    readInferOption = fmap parseInfer str
    -- TODO: Make this more robust
    -- This can encounter a read error, we should
    -- ensure that we can't reach this point
    -- with invalid mode.
    parseInfer = read . (++"Mode")


stencilsSynthOptions :: Parser StencilsSynthOptions
stencilsSynthOptions = fmap StencilsSynthOptions
  stencilsOptions <*> annotationOptions


unitsOptions :: Parser UnitsOptions
unitsOptions = fmap UnitsOptions
      readOptions
  <*> literalsOption
  <*> debugOption
  <*> optional includeDirOption
  where
    literalsOption = option parseLiterals $
                     long "units-literals"
                     <> short 'l'
                     <> metavar "ID"
                     <> completeWith ["Unitless", "Poly", "Mixed"]
                     <> value LitMixed
                     <> help "units-of-measure literals mode. ID = Unitless, Poly, or Mixed"
    parseLiterals = fmap read str
    debugOption = switch (long "debug" <> help "enable debug mode")
    dirOption m = strOption (metavar "DIR" <> action "directory" <> m)
    includeDirOption = dirOption
      (   long "include-dir"
       <> short 'I'
       <> help "directory to search for precompiled files")


unitsWriteOptions :: Parser UnitsWriteOptions
unitsWriteOptions = fmap UnitsWriteOptions
  unitsOptions <*> writeOptions


annotationOptions :: Parser AnnotationOptions
annotationOptions = fmap AnnotationOptions $
  flag ATDefault Doxygen
    (long "doxygen" <> help "synthesise annotations compatible with Doxygen")
  <|> flag ATDefault Ford
    (long "ford" <> help "synthesise annotations compatible with Ford")


unitsSynthOptions :: Parser UnitsSynthOptions
unitsSynthOptions = fmap UnitsSynthOptions
  unitsWriteOptions <*> annotationOptions


refactOptions :: Parser RefactOptions
refactOptions = fmap RefactOptions
  readOptions <*> writeOptions


cmdCount, cmdAST :: Parser Command
cmdCount = fmap CmdCount readOptions
cmdAST   = fmap CmdAST   readOptions


cmdStencilsCheck, cmdStencilsInfer, cmdStencilsSynth :: Parser Command
cmdStencilsCheck = fmap CmdStencilsCheck readOptions
cmdStencilsInfer = fmap CmdStencilsInfer stencilsOptions
cmdStencilsSynth = fmap CmdStencilsSynth stencilsSynthOptions


cmdUnitsSuggest, cmdUnitsCheck, cmdUnitsInfer
  , cmdUnitsSynth, cmdUnitsCompile :: Parser Command
cmdUnitsSuggest = fmap CmdUnitsSuggest unitsWriteOptions
cmdUnitsCheck   = fmap CmdUnitsCheck   unitsOptions
cmdUnitsInfer   = fmap CmdUnitsInfer   unitsOptions
cmdUnitsSynth   = fmap CmdUnitsSynth   unitsSynthOptions
cmdUnitsCompile = fmap CmdUnitsCompile unitsWriteOptions


cmdRefactCommon, cmdRefactDatatype
  , cmdRefactDead, cmdRefactEquivalence :: Parser Command
cmdRefactCommon      = fmap CmdRefactCommon refactOptions
cmdRefactDatatype    = fmap CmdRefactDatatype refactOptions
cmdRefactDead        = fmap CmdRefactDead refactOptions
cmdRefactEquivalence = fmap CmdRefactEquivalence refactOptions


analysesParser :: Parser Command
analysesParser = hsubparser $
     (command "count" . info cmdCount
       . progDesc $ "count variable declarations")
  <> (command "ast" . info cmdAST
       . progDesc $ "print the raw AST -- for development purposes")
  <> (command "stencils-check" . info cmdStencilsCheck
       . progDesc $ "stencil spec checking")
  <> (command "stencils-infer" . info cmdStencilsInfer
       . progDesc $ "stencil spec inference")
  <> (command "stencils-synth" . info cmdStencilsSynth
       . progDesc $ "stencil spec synthesis")
  <> (command "units-suggest" . info cmdUnitsSuggest
       . progDesc $ "suggest variables to annotate with units-of-measure for maximum coverage")
  <> (command "units-check" . info cmdUnitsCheck
       . progDesc $ "unit-of-measure checking")
  <> (command "units-infer" . info cmdUnitsInfer
       . progDesc $ "unit-of-measure inference")
  <> (command "units-synth" . info cmdUnitsSynth
       . progDesc $ "unit-of-measure synthesise specs")
  <> (command "units-compile" . info cmdUnitsCompile
       . progDesc $ "units-of-measure compile module information")
  <> commandGroup "Analysis Commands"


refactoringsParser :: Parser Command
refactoringsParser = hsubparser $
     (command "common" . info cmdRefactCommon
       . progDesc $ "common block elimination")
  <> (command "equivalence" . info cmdRefactEquivalence
       . progDesc $ "equivalence elimination")
  <> (command "dead" . info cmdRefactDead
       . progDesc $ "dead-code elimination")
  <> (command "datatype" . info cmdRefactDatatype
       . progDesc $ "derived data type introduction")
  <> commandGroup "Refactoring Commands"


topLevelCommands :: Parser Command
topLevelCommands = versionOption
  where versionOption = pure CmdTopVersion <* switch
                        (  long "version"
                        <> short 'v'
                        <> short '?'
                        <> help "show version number")


-- | Collective parser for all CamFort commands.
commandParser :: Parser Command
commandParser =
  helper <*> (analysesParser <|> refactoringsParser <|> topLevelCommands)


main :: IO ()
main = do
  cmd <- execParser (info commandParser idm)
  runCommand cmd
  where
    getExcludes = fromMaybe [] . exclude
    getOutputFile _ (WriteFile f) = f
    getOutputFile inp WriteInplace = inp
    runRO ro f = f (inputSource ro) (getExcludes ro)
    runSO so f =
      let ro     = soReadOptions so
          wo     = soWriteOptions so
          inFile = inputSource ro
      in runRO ro f (getOutputFile inFile wo) (soInferMode so)
    runSSO sso f =
      let ao = ssoAnnotationOptions sso
          so = ssoStencilsOptions sso
      in runSO so stencilsSynth (annotationType ao)
    runUO uo f =
      let ro = uoReadOptions uo
      in runRO ro f (literals uo) (debug uo) (includeDir uo)
    runUWO uwo f =
      let uo     = uwoUnitsOptions uwo
          ro     = uoReadOptions uo
          wo     = uwoWriteOptions uwo
          inFile = inputSource ro
      in runUO uo f (getOutputFile inFile wo)
    runUSO uso f =
      let uwo = usoUnitsWriteOptions uso
          ao  = usoAnnotationOptions uso
      in runUWO uwo f (annotationType ao)
    runRFO rfo f =
      let ro     = rfoReadOptions rfo
          wo     = rfoWriteOptions rfo
          inFile = inputSource ro
      in runRO ro f (getOutputFile inFile wo)
    runCommand (CmdAST ro)                = runRO ro ast
    runCommand (CmdCount ro)              = runRO ro countVarDecls
    runCommand (CmdStencilsCheck ro)      = runRO ro stencilsCheck
    runCommand (CmdStencilsInfer so)      = runSO so stencilsInfer
    runCommand (CmdStencilsSynth sso)     = runSSO sso stencilsSynth
    runCommand (CmdUnitsSuggest uwo)      = runUWO uwo unitsCriticals
    runCommand (CmdUnitsCheck uo)         = runUO uo unitsCheck
    runCommand (CmdUnitsInfer uo)         = runUO uo unitsInfer
    runCommand (CmdUnitsSynth uso)        = runUSO uso unitsSynth
    runCommand (CmdUnitsCompile uwo)      = runUWO uwo unitsCompile
    runCommand (CmdRefactCommon rfo)      = runRFO rfo common
    runCommand (CmdRefactDead rfo)        = runRFO rfo dead
    runCommand (CmdRefactEquivalence rfo) = runRFO rfo equivalences
    runCommand (CmdRefactDatatype rfo)    = runRFO rfo datatypes
    runCommand CmdTopVersion              = displayVersion


-- | Current CamFort version.
version = "0.903"


-- | Full CamFort version string.
versionMessage = "CamFort " ++ version ++ " - Cambridge Fortran Infrastructure."


-- | Print the full version string.
displayVersion :: IO ()
displayVersion = putStrLn versionMessage
