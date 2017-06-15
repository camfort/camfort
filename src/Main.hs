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
readOptions = liftA ReadOptions
  (fileArgument $ help "input file")
  <*> excludeOption


writeOptions :: Parser WriteOptions
writeOptions = (liftA WriteFile . fileArgument $
                 help "file to write output to")
               <|> (pure WriteInplace <* switch
                     (   long "inplace"
                      <> help "write in place (replaces input files)"))


stencilsOptions :: Parser StencilsOptions
stencilsOptions = liftA StencilsOptions
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
stencilsSynthOptions = liftA StencilsSynthOptions
  stencilsOptions <*> annotationOptions


unitsOptions :: Parser UnitsOptions
unitsOptions = liftA UnitsOptions
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
unitsWriteOptions = liftA UnitsWriteOptions
  unitsOptions <*> writeOptions


annotationOptions :: Parser AnnotationOptions
annotationOptions = liftA AnnotationOptions $
  flag ATDefault Doxygen
    (long "doxygen" <> help "synthesise annotations compatible with Doxygen")
  <|> flag ATDefault Ford
    (long "ford" <> help "synthesise annotations compatible with Ford")


unitsSynthOptions :: Parser UnitsSynthOptions
unitsSynthOptions = liftA UnitsSynthOptions
  unitsWriteOptions <*> annotationOptions


refactOptions :: Parser RefactOptions
refactOptions = liftA RefactOptions
  readOptions <*> writeOptions


cmdCount, cmdAST :: Parser Command
cmdCount = liftA CmdCount readOptions
cmdAST   = liftA CmdAST   readOptions


cmdStencilsCheck, cmdStencilsInfer, cmdStencilsSynth :: Parser Command
cmdStencilsCheck = liftA CmdStencilsCheck readOptions
cmdStencilsInfer = liftA CmdStencilsInfer stencilsOptions
cmdStencilsSynth = liftA CmdStencilsSynth stencilsSynthOptions


cmdUnitsSuggest, cmdUnitsCheck, cmdUnitsInfer
  , cmdUnitsSynth, cmdUnitsCompile :: Parser Command
cmdUnitsSuggest = liftA CmdUnitsSuggest unitsWriteOptions
cmdUnitsCheck   = liftA CmdUnitsCheck   unitsOptions
cmdUnitsInfer   = liftA CmdUnitsInfer   unitsOptions
cmdUnitsSynth   = liftA CmdUnitsSynth   unitsSynthOptions
cmdUnitsCompile = liftA CmdUnitsCompile unitsWriteOptions


cmdRefactCommon, cmdRefactDatatype
  , cmdRefactDead, cmdRefactEquivalence :: Parser Command
cmdRefactCommon      = liftA CmdRefactCommon refactOptions
cmdRefactDatatype    = liftA CmdRefactDatatype refactOptions
cmdRefactDead        = liftA CmdRefactDead refactOptions
cmdRefactEquivalence = liftA CmdRefactEquivalence refactOptions


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
    runCommand (CmdAST ro) =
      ast (inputSource ro) (getExcludes ro)
    runCommand (CmdCount ro) =
      countVarDecls (inputSource ro) (getExcludes ro)
    runCommand (CmdStencilsCheck ro) =
      stencilsCheck (inputSource ro) (getExcludes ro)
    runCommand (CmdStencilsInfer so) =
      let ro = soReadOptions so
          wo = soWriteOptions so
          inFile = inputSource ro
      in stencilsInfer inFile (getExcludes ro)
                       (getOutputFile inFile wo) (soInferMode so)
    runCommand (CmdStencilsSynth sso) =
      let so = ssoStencilsOptions sso
          ro = soReadOptions so
          wo = soWriteOptions so
          ao = ssoAnnotationOptions sso
          inFile = inputSource ro
      in stencilsSynth inFile (getExcludes ro)
                       (getOutputFile inFile wo) (soInferMode so)
                       (annotationType ao)
    runCommand (CmdUnitsSuggest uwo) =
      let uo = uwoUnitsOptions uwo
          ro = uoReadOptions uo
          wo = uwoWriteOptions uwo
          inFile = inputSource ro
      in unitsCriticals inFile (getExcludes ro)
                       (getOutputFile inFile wo)
                       (literals uo) (debug uo)
                       (includeDir uo)
    runCommand (CmdUnitsCheck uo) =
      let ro = uoReadOptions uo
          inFile = inputSource ro
      in unitsCheck inFile (getExcludes ro)
                       (literals uo) (debug uo)
                       (includeDir uo)
    runCommand (CmdUnitsInfer uo) =
      let ro = uoReadOptions uo
          inFile = inputSource ro
      in unitsInfer inFile (getExcludes ro)
                       (literals uo) (debug uo)
                       (includeDir uo)
    runCommand (CmdUnitsSynth uso) =
      let uwo = usoUnitsWriteOptions uso
          uo = uwoUnitsOptions uwo
          ro = uoReadOptions uo
          wo = uwoWriteOptions uwo
          ao = usoAnnotationOptions uso
          inFile = inputSource ro
      in unitsSynth inFile (getExcludes ro)
                       (getOutputFile inFile wo)
                       (annotationType ao)
                       (literals uo) (debug uo)
                       (includeDir uo)
    runCommand (CmdUnitsCompile uwo) =
      let uo = uwoUnitsOptions uwo
          ro = uoReadOptions uo
          wo = uwoWriteOptions uwo
          inFile = inputSource ro
      in unitsCompile inFile (getExcludes ro)
                       (getOutputFile inFile wo)
                       (literals uo) (debug uo)
                       (includeDir uo)
    runCommand (CmdRefactCommon rfo) =
      let ro = rfoReadOptions rfo
          wo = rfoWriteOptions rfo
          inFile = inputSource ro
      in common inFile (getExcludes ro) (getOutputFile inFile wo)
    runCommand (CmdRefactDead rfo) =
      let ro = rfoReadOptions rfo
          wo = rfoWriteOptions rfo
          inFile = inputSource ro
      in dead inFile (getExcludes ro) (getOutputFile inFile wo)
    runCommand (CmdRefactEquivalence rfo) =
      let ro = rfoReadOptions rfo
          wo = rfoWriteOptions rfo
          inFile = inputSource ro
      in equivalences inFile (getExcludes ro) (getOutputFile inFile wo)
    runCommand (CmdRefactDatatype rfo) =
      let ro = rfoReadOptions rfo
          wo = rfoWriteOptions rfo
          inFile = inputSource ro
      in datatypes inFile (getExcludes ro) (getOutputFile inFile wo)
    runCommand CmdTopVersion = displayVersion


-- | Current CamFort version.
version = "0.903"


-- | Full CamFort version string.
versionMessage = "CamFort " ++ version ++ " - Cambridge Fortran Infrastructure."


-- | Print the full version string.
displayVersion :: IO ()
displayVersion = putStrLn versionMessage
