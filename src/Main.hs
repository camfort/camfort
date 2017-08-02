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

module Main (main) where

import Camfort.Input (defaultValue)
import Camfort.Functionality
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Directory (getCurrentDirectory)

import Options.Applicative


-- | Commands supported by CamFort.
data Command = CmdCount ReadOptions
             | CmdAST ReadOptions
             | CmdStencilsCheck ReadOptions
             | CmdStencilsInfer StencilsInferOptions
             | CmdStencilsSynth StencilsSynthOptions
             | CmdUnitsSuggest UnitsOptions
             | CmdUnitsCheck UnitsOptions
             | CmdUnitsInfer UnitsOptions
             | CmdUnitsSynth UnitsSynthOptions
             | CmdRefactCommon RefactOptions
             | CmdRefactDead RefactOptions
             | CmdRefactEquivalence RefactOptions
             | CmdInit FilePath
             | CmdTopVersion


-- | Options for reading files.
data ReadOptions = ReadOptions
  { inputSource :: String
  , includeDir  :: Maybe String
  , exclude     :: Maybe [String]
  }


-- | Options for writing to files.
--
-- User can choose to either specify which file to write, or have
-- the input files be written over.
data WriteOptions = WriteFile { _outputFile :: String }
                  | WriteInplace


-- | Options used by all unit commands.
data UnitsOptions = UnitsOptions
  { uoReadOptions :: ReadOptions
  , literals      :: LiteralsOpt
  , debug         :: Bool
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


data StencilsInferOptions = StencilsInferOptions
  { sioReadOptions :: ReadOptions
  , sioUseEval     :: Bool
  }


data StencilsSynthOptions = StencilsSynthOptions
  { ssoReadOptions        :: ReadOptions
  , ssoWriteOptions       :: WriteOptions
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

-- | Parser for an argument representing an individual directory.
directoryArgument :: Mod ArgumentFields String -> Parser String
directoryArgument m = strArgument (metavar "DIRECTORY" <> action "directory" <> m)

-- | Parser for file options with multiple files specified
-- | as a comma-separated list.
multiFileOption :: Mod OptionFields [String] -> Parser [String]
multiFileOption m = option (list str)
                    (metavar "FILE..." <> action "file" <> m)
  where list :: ReadM String -> ReadM [String]
        list = fmap (splitBy ',')
        splitBy _ [] = []
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
  <*> optional includeDirOption
  <*> excludeOption
  where
    dirOption m = strOption (metavar "DIR" <> action "directory" <> m)
    includeDirOption = dirOption
      (   long "include-dir"
       <> short 'I'
       <> help "directory to search for precompiled files")


-- | User must specify either an ouput file, or say that the file
-- | should be rewritten in place.
writeOptions :: Parser WriteOptions
writeOptions = (fmap WriteFile . fileArgument $
                 help "file to write output to")
               <|> (pure WriteInplace <* flag' ()
                     (   long "inplace"
                      <> help "write in place (replaces input files)"))


stencilsInferOptions :: Parser StencilsInferOptions
stencilsInferOptions = fmap StencilsInferOptions
  readOptions <*> evalOption
  where
    evalOption = switch
      (    long "eval"
        <> help "provide additional evaluation reporting"
        <> internal)


stencilsSynthOptions :: Parser StencilsSynthOptions
stencilsSynthOptions = fmap StencilsSynthOptions
  readOptions <*> writeOptions <*> annotationOptions


unitsOptions :: Parser UnitsOptions
unitsOptions = fmap UnitsOptions
      readOptions
  <*> literalsOption
  <*> debugOption
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
cmdStencilsInfer = fmap CmdStencilsInfer stencilsInferOptions
cmdStencilsSynth = fmap CmdStencilsSynth stencilsSynthOptions


cmdUnitsSuggest, cmdUnitsCheck, cmdUnitsInfer
  , cmdUnitsSynth :: Parser Command
cmdUnitsSuggest = fmap CmdUnitsSuggest unitsOptions
cmdUnitsCheck   = fmap CmdUnitsCheck   unitsOptions
cmdUnitsInfer   = fmap CmdUnitsInfer   unitsOptions
cmdUnitsSynth   = fmap CmdUnitsSynth   unitsSynthOptions


cmdRefactCommon, cmdRefactDead, cmdRefactEquivalence :: Parser Command
cmdRefactCommon      = fmap CmdRefactCommon refactOptions
cmdRefactDead        = fmap CmdRefactDead refactOptions
cmdRefactEquivalence = fmap CmdRefactEquivalence refactOptions

-- | Helper for building a command alias.
--
-- Command aliases will not show up in the help text, nor be subject to completion.
commandAlias :: String -> Parser Command -> Mod CommandFields Command
commandAlias alias cmdParser = command alias . info cmdParser $ mempty

-- | Helper for building a parser for a group of commands.
commandsParser :: String -> [(String, [String], Parser Command, String)] -> Parser Command
commandsParser groupName commands =
  hsubparser (mconcat (fmap
                        (\(name, _, cmdParser, description) ->
                           (command name . info cmdParser . progDesc $ description))
                        commands)
  <> commandGroup groupName)
  <|> aliasSubParser
  where aliasSubParser = subparser $
          mconcat (fmap
                    (\(_, aliases, cmdParser, _) ->
                       mconcat $ fmap (`commandAlias` cmdParser) aliases)
                    commands)
          <> internal

analysesParser :: Parser Command
analysesParser = commandsParser "Analysis Commands" analysesCommands
  where
    analysesCommands =
      [ ("count",
          [],
          cmdCount,         "count variable declarations")
      , ("ast",
          [],
          cmdAST,           "print the raw AST -- for development purposes")
      , ("stencils-check",
          ["stencil-check", "check-stencils", "check-stencil"],
          cmdStencilsCheck, "stencil spec checking")
      , ("stencils-infer",
          ["stencil-infer", "infer-stencils", "infer-stencil"],
          cmdStencilsInfer, "stencil spec inference")
      , ("stencils-synth",
          ["stencil-synth", "synth-stencils", "synth-stencil"],
          cmdStencilsSynth, "stencil spec synthesis")
      , ("units-suggest",
          ["unit-suggest", "suggest-units", "suggest-unit"],
          cmdUnitsSuggest,  "suggest variables to annotate with units-of-measure for maximum coverage")
      , ("units-check",
          ["unit-check", "check-units", "check-unit"],
          cmdUnitsCheck,    "unit-of-measure checking")
      , ("units-infer",
          ["unit-infer", "infer-units", "infer-unit"],
          cmdUnitsInfer,    "unit-of-measure inference")
      , ("units-synth",
          ["unit-synth", "synth-units", "synth-unit"],
          cmdUnitsSynth,    "unit-of-measure synthesise specs") ]


refactoringsParser :: Parser Command
refactoringsParser = commandsParser "Refactoring Commands" refactoringsCommands
  where
    refactoringsCommands =
      [ ("common",      [], cmdRefactCommon,      "common block elimination")
      , ("equivalence", [], cmdRefactEquivalence, "equivalence elimination")
      , ("dead",        [], cmdRefactDead,        "dead-code elimination") ]


topLevelCommands :: Parser Command
topLevelCommands = versionOption
  where versionOption = pure CmdTopVersion <* switch
                        (  long "version"
                        <> short 'v'
                        <> short '?'
                        <> help "show version number")

cmdInit :: FilePath -> Parser Command
cmdInit currDir = fmap CmdInit . directoryArgument $
  help "project directory" <> value currDir

projectParser :: FilePath -> Parser Command
projectParser currDir = commandsParser "Project Commands" projectCommands
  where
    projectCommands =
      [ ("init", [], cmdInit currDir, "initialize CamFort for the project") ]

-- | Collective parser for all CamFort commands.
commandParser :: FilePath -> Parser Command
commandParser currDir =
  helper <*> (    projectParser currDir
              <|> analysesParser
              <|> refactoringsParser
              <|> topLevelCommands)

main :: IO ()
main = do
  currDir <- getCurrentDirectory
  cmd <- execParser (info (commandParser currDir) idm)
  runCommand cmd
  where
    getExcludes = fromMaybe [] . exclude
    getOutputFile _ (WriteFile f) = f
    getOutputFile inp WriteInplace = inp
    runRO ro f = f (inputSource ro) (includeDir ro) (getExcludes ro)
    runSIO sio f =
      let ro      = sioReadOptions sio
          useEval = sioUseEval sio
          inFile  = inputSource ro
      in runRO ro f useEval
    runSSO sso f =
      let ao     = ssoAnnotationOptions sso
          wo     = ssoWriteOptions sso
          ro     = ssoReadOptions sso
          inFile = inputSource ro
      in runRO ro f (annotationType ao) (getOutputFile inFile wo)
    runUO uo f =
      let ro = uoReadOptions uo
      in runRO ro f (literals uo) (debug uo)
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
    runCommand (CmdStencilsInfer so)      = runSIO so stencilsInfer
    runCommand (CmdStencilsSynth sso)     = runSSO sso stencilsSynth
    runCommand (CmdUnitsSuggest uo)       = runUO uo unitsCriticals
    runCommand (CmdUnitsCheck uo)         = runUO uo unitsCheck
    runCommand (CmdUnitsInfer uo)         = runUO uo unitsInfer
    runCommand (CmdUnitsSynth uso)        = runUSO uso unitsSynth
    runCommand (CmdRefactCommon rfo)      = runRFO rfo common
    runCommand (CmdRefactDead rfo)        = runRFO rfo dead
    runCommand (CmdRefactEquivalence rfo) = runRFO rfo equivalences
    runCommand (CmdInit dir)              = camfortInitialize dir
    runCommand CmdTopVersion              = displayVersion


-- | Current CamFort version.
version = "0.905"


-- | Full CamFort version string.
versionMessage = "CamFort " ++ version ++ " - Cambridge Fortran Infrastructure."


-- | Print the full version string.
displayVersion :: IO ()
displayVersion = putStrLn versionMessage
