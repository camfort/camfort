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

{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables #-}

module Main (main) where

import Camfort.Analysis.Logger (LogLevel(..))
import Camfort.Functionality
import Camfort.Specification.Hoare (PrimReprOption(..))
import Camfort.Specification.Units.Monad (LiteralsOpt(LitMixed))
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import GHC.Stack
import Language.Fortran.Version (FortranVersion(..), selectFortranVersion)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.Exit

main :: IO ()
main = catch realMain stacktrace
  where
    stacktrace (e::AsyncException) = do
      putStrLn "Call stack:"
      stack <- currentCallStack
      mapM_ putStrLn stack
      putStrLn "Exception:"
      print e

realMain :: IO ()
realMain = do
  currentDir <- getCurrentDirectory
  cmd <- customExecParser (prefs showHelpOnEmpty) (info (commandParser currentDir) idm)
  code <- runCommand cmd
  if code == 0
    then exitWith ExitSuccess
    else exitWith $ ExitFailure code
  where
    env ro lo = CamfortEnv
      { ceInputSources   = inputSource ro
      , ceIncludeDir     = includeDir ro
      , ceExcludeFiles   = getExcludes ro
      , ceLogLevel       = logLevel lo
      , ceSourceSnippets = snippets lo
      , ceFortranVersion = fortranVersion ro
      }

    getExcludes = fromMaybe [] . exclude

    getOutputFile _ (WriteFile f) = f
    getOutputFile inp WriteInplace = inp

    runRO ro lo f = f (env ro lo)
    runSIO sio f =
      let ro      = sioReadOptions sio
          lo      = sioLogOptions sio
          useEval = sioUseEval sio
      in runRO ro lo (f useEval)
    runSSO sso f =
      let ao     = ssoAnnotationOptions sso
          wo     = ssoWriteOptions sso
          ro     = ssoReadOptions sso
          lo     = ssoLogOptions sso
          inFile = inputSource ro
      in runRO ro lo (f (annotationType ao) (getOutputFile inFile wo))
    runDSO dso f =
      let ao     = dsoAnnotationOptions dso
          wo     = dsoWriteOptions dso
          ro     = dsoReadOptions dso
          lo     = dsoLogOptions dso
          inFile = inputSource ro
      in runRO ro lo (f (annotationType ao) (getOutputFile inFile wo))
    runUO uo f =
      let ro = uoReadOptions uo
          lo = uoLogOptions uo
      in runRO ro lo (f (literals uo) (includeUninitialized uo))
    runUWO uwo f =
      let uo     = uwoUnitsOptions uwo
          ro     = uoReadOptions uo
          wo     = uwoWriteOptions uwo
          inFile = inputSource ro
      in runUO uo (f (getOutputFile inFile wo))
    runUSO uso f =
      let uwo = usoUnitsWriteOptions uso
          ao  = usoAnnotationOptions uso
      in runUWO uwo (f (annotationType ao))
    runIO io f =
      let ro = ioReadOptions io
          lo = ioLogOptions io
          pro = ioPrimReprOption io
      in runRO ro lo (f pro)
    runRFO rfo f =
      let ro     = rfoReadOptions rfo
          lo     = rfoLogOptions rfo
          wo     = rfoWriteOptions rfo
          inFile = inputSource ro
      in runRO ro lo (f (getOutputFile inFile wo))

    runCommand :: Command -> IO Int
    runCommand (CmdAST ro lo)             = runRO ro lo ast >> return 0
    runCommand (CmdCount ro lo)           = runRO ro lo countVarDecls
    runCommand (CmdStencilsCheck ro lo)   = runRO ro lo stencilsCheck
    runCommand (CmdStencilsInfer so)      = runSIO so stencilsInfer
    runCommand (CmdStencilsSynth sso)     = runSSO sso stencilsSynth
    runCommand (CmdUnitsSuggest uo)       = runUO uo unitsCriticals
    runCommand (CmdUnitsCheck uo)
      | uoDumpMode uo                     = runUO uo unitsDump
      | otherwise                         = runUO uo unitsCheck
    runCommand (CmdUnitsInfer uo)         = runUO uo $ unitsInfer (uoShowAST uo)
    runCommand (CmdUnitsSynth uso)        = runUSO uso unitsSynth
    runCommand (CmdUnitsCompile uo)       = runUO uo unitsCompile
    runCommand (CmdInvariantsCheck io)    = runIO io invariantsCheck
    runCommand (CmdRefactCommon rfo)      = runRFO rfo common
    runCommand (CmdRefactDead rfo)        = runRFO rfo dead
    runCommand (CmdRefactEquivalence rfo) = runRFO rfo equivalences
    runCommand (CmdInferDDT ro lo)        = runRO ro lo ddtInfer
    runCommand (CmdSynthDDT dso)          = runDSO dso ddtSynth
    runCommand (CmdCheckDDT ro lo)        = runRO ro lo ddtCheck
    runCommand (CmdRefactDDT rfo)         = runRFO rfo ddtRefactor
    runCommand (CmdCompileDDT ro lo)      = runRO ro lo ddtCompile
    runCommand (CmdImplicitNone ro lo)    = runRO ro lo (implicitNone False)
    runCommand (CmdImplicitNoneAll ro lo) = runRO ro lo (implicitNone True)
    runCommand (CmdAllocCheck ro lo)      = runRO ro lo allocCheck
    runCommand (CmdFPCheck ro lo)         = runRO ro lo fpCheck
    runCommand (CmdUseCheck ro lo)        = runRO ro lo useCheck
    runCommand (CmdArrayCheck ro lo)      = runRO ro lo arrayCheck
    runCommand (CmdBasicChecks ro lo)     = maximum <$> mapM (runRO ro lo) basicChecks
    runCommand (CmdInit dir)              = camfortInitialize dir >> return 0
    runCommand CmdTopVersion              = displayVersion >> return 0

basicChecks :: [CamfortEnv -> IO Int]
basicChecks = [allocCheck, arrayCheck, fpCheck, implicitNone False, useCheck]

-- | Commands supported by CamFort.
data Command = CmdCount ReadOptions LogOptions
             | CmdAST ReadOptions LogOptions
             | CmdStencilsCheck ReadOptions LogOptions
             | CmdStencilsInfer StencilsInferOptions
             | CmdStencilsSynth StencilsSynthOptions
             | CmdUnitsSuggest UnitsOptions
             | CmdUnitsCheck UnitsOptions
             | CmdUnitsInfer UnitsOptions
             | CmdUnitsCompile UnitsOptions
             | CmdUnitsSynth UnitsSynthOptions
             | CmdInvariantsCheck InvariantsOptions
             | CmdRefactCommon RefactOptions
             | CmdRefactDead RefactOptions
             | CmdRefactEquivalence RefactOptions
             | CmdInferDDT ReadOptions LogOptions
             | CmdSynthDDT DDTSynthOptions
             | CmdCheckDDT ReadOptions LogOptions
             | CmdRefactDDT RefactOptions
             | CmdCompileDDT ReadOptions LogOptions
             | CmdImplicitNone ReadOptions LogOptions
             | CmdImplicitNoneAll ReadOptions LogOptions
             | CmdAllocCheck ReadOptions LogOptions
             | CmdFPCheck ReadOptions LogOptions
             | CmdUseCheck ReadOptions LogOptions
             | CmdArrayCheck ReadOptions LogOptions
             | CmdBasicChecks ReadOptions LogOptions
             | CmdInit FilePath
             | CmdTopVersion


-- | Options for reading files.
data ReadOptions = ReadOptions
  { inputSource :: String
  , includeDir  :: Maybe String
  , exclude     :: Maybe [String]
  , fortranVersion :: Maybe FortranVersion
  }


data LogOptions = LogOptions
  { logLevel :: LogLevel
  , snippets :: Bool
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
  , uoLogOptions  :: LogOptions
  , literals      :: LiteralsOpt
  , uoDumpMode    :: Bool
  , uoShowAST     :: Bool
  , includeUninitialized :: Bool
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
  , sioLogOptions  :: LogOptions
  , sioUseEval     :: Bool
  }


data StencilsSynthOptions = StencilsSynthOptions
  { ssoReadOptions        :: ReadOptions
  , ssoLogOptions         :: LogOptions
  , ssoWriteOptions       :: WriteOptions
  , ssoAnnotationOptions  :: AnnotationOptions
  }

data DDTSynthOptions = DDTSynthOptions
  { dsoReadOptions        :: ReadOptions
  , dsoLogOptions         :: LogOptions
  , dsoWriteOptions       :: WriteOptions
  , dsoAnnotationOptions  :: AnnotationOptions
  }

data InvariantsOptions = InvariantsOptions
  { ioReadOptions :: ReadOptions
  , ioLogOptions :: LogOptions
  , ioPrimReprOption :: PrimReprOption
  }


-- | Options used by refactoring commands.
data RefactOptions = RefactOptions
  { rfoReadOptions  :: ReadOptions
  , rfoLogOptions   :: LogOptions
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


excludeOption :: Parser [String]
excludeOption = multiFileOption $
     long "exclude"
  <> short 'e'
  <> help "files to exclude (comma separated list, no spaces)"

fortranVersionOption :: Parser FortranVersion
fortranVersionOption = option parseFortranVersion $
     long "fortranVersion"
  <> short 'F'
  <> metavar "VERSION"
  <> help "version of Fortran to parse"
  where
    parseFortranVersion = eitherReader $ \ s -> case selectFortranVersion s of
      Just v  -> Right v
      Nothing -> Left "unable to parse the supplied Fortran version string"

-- | Parse options for 'ReadOptions'.
readOptions :: Parser ReadOptions
readOptions = fmap ReadOptions
  (fileArgument $ help "input file")
  <*> optional includeDirOption
  <*> optional excludeOption
  <*> optional fortranVersionOption
  where
    dirOption m = strOption (metavar "DIR" <> action "directory" <> m)
    includeDirOption = dirOption
      (   long "include-dir"
       <> short 'I'
       <> help "directory to search for precompiled files")


logOptions :: Parser LogOptions
logOptions = LogOptions <$> logLevelOption <*> snippetsOption
  where
    logLevelOption =
      let toLogLevel debug = if debug then LogDebug else LogInfo
      in toLogLevel <$> switch (long "debug" <> help "enable debug output")
    snippetsOption = switch (long "snippets" <> help "show code snippets")


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
  readOptions <*> logOptions <*> evalOption
  where
    evalOption = switch
      (    long "eval"
        <> help "provide additional evaluation reporting"
        <> internal)


stencilsSynthOptions :: Parser StencilsSynthOptions
stencilsSynthOptions = fmap StencilsSynthOptions
  readOptions <*> logOptions <*> writeOptions <*> annotationOptions


unitsOptions :: Parser UnitsOptions
unitsOptions = fmap UnitsOptions
      readOptions
  <*> logOptions
  <*> literalsOption
  <*> dumpModFileOption
  <*> showASTOption
  <*> pure False
  where
    literalsOption = option parseLiterals $
                     long "units-literals"
                     <> short 'l'
                     <> metavar "ID"
                     <> completeWith ["Unitless", "Poly", "Mixed"]
                     <> value LitMixed
                     <> help "units-of-measure literals mode. ID = Unitless, Poly, or Mixed"
    dumpModFileOption = switch (long "dump-mod-file" <> help "show contents of fsmod file")
    showASTOption = switch (long "show-ast" <> help "show units at each AST node")
    parseLiterals = fmap read str

unitsSuggestOptions :: Parser UnitsOptions
unitsSuggestOptions =
    (fmap (\f b -> f { includeUninitialized = b }) unitsOptions) <*> uninitOption
  where
    uninitOption = switch (long "include-uninit" <> help "include suggestions for uninitialized variables")

invariantsOptions :: Parser InvariantsOptions
invariantsOptions = fmap InvariantsOptions
      readOptions
  <*> logOptions
  <*> reprOption
  where
    reprOption =
      flag PROIdealized PROPrecise
      $  long "precise-reprs"
      <> short 'p'
      <> help "use precise data representations, in invariant verification, at\
              \the cost of speed and provability; see documentation in\
              \Language.Model.Fortran.Repr.Prim"


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
  readOptions <*> logOptions <*> writeOptions

ddtSynthOptions :: Parser DDTSynthOptions
ddtSynthOptions = fmap DDTSynthOptions
  readOptions <*> logOptions <*> writeOptions <*> annotationOptions

cmdCount, cmdAST :: Parser Command
cmdCount        = fmap CmdCount readOptions <*> logOptions
cmdAST          = fmap CmdAST   readOptions <*> logOptions


cmdStencilsCheck, cmdStencilsInfer, cmdStencilsSynth :: Parser Command
cmdStencilsCheck = fmap CmdStencilsCheck readOptions <*> logOptions
cmdStencilsInfer = fmap CmdStencilsInfer stencilsInferOptions
cmdStencilsSynth = fmap CmdStencilsSynth stencilsSynthOptions


cmdUnitsSuggest, cmdUnitsCheck, cmdUnitsInfer, cmdUnitsCompile, cmdUnitsSynth :: Parser Command
cmdUnitsSuggest = fmap CmdUnitsSuggest unitsSuggestOptions
cmdUnitsCheck   = fmap CmdUnitsCheck   unitsOptions
cmdUnitsInfer   = fmap CmdUnitsInfer   unitsOptions
cmdUnitsCompile = fmap CmdUnitsCompile unitsOptions
cmdUnitsSynth   = fmap CmdUnitsSynth   unitsSynthOptions


cmdImplicitNone, cmdImplicitNoneAll, cmdInvariantsCheck, cmdAllocCheck, cmdFPCheck :: Parser Command
cmdArrayCheck, cmdUseCheck, cmdBasicChecks :: Parser Command
cmdInvariantsCheck = fmap CmdInvariantsCheck invariantsOptions
cmdImplicitNone    = fmap CmdImplicitNone readOptions <*> logOptions
cmdImplicitNoneAll = fmap CmdImplicitNoneAll readOptions <*> logOptions
cmdAllocCheck      = fmap CmdAllocCheck readOptions <*> logOptions
cmdFPCheck         = fmap CmdFPCheck readOptions <*> logOptions
cmdUseCheck        = fmap CmdUseCheck readOptions <*> logOptions
cmdArrayCheck      = fmap CmdArrayCheck readOptions <*> logOptions
cmdBasicChecks     = fmap CmdBasicChecks readOptions <*> logOptions

cmdRefactCommon, cmdRefactDead, cmdRefactEquivalence :: Parser Command
cmdRefactCommon      = fmap CmdRefactCommon refactOptions
cmdRefactDead        = fmap CmdRefactDead refactOptions
cmdRefactEquivalence = fmap CmdRefactEquivalence refactOptions

cmdInferDDT, cmdSynthDDT, cmdCheckDDT, cmdRefactDDT, cmdCompileDDT :: Parser Command
cmdInferDDT = fmap CmdInferDDT readOptions <*> logOptions
cmdSynthDDT = fmap CmdSynthDDT ddtSynthOptions
cmdCheckDDT = fmap CmdCheckDDT readOptions <*> logOptions
cmdRefactDDT = fmap CmdRefactDDT refactOptions
cmdCompileDDT = fmap CmdCompileDDT readOptions <*> logOptions

-- | Helper for building a command alias.
--
-- Command aliases will not show up in the help text, nor be subject to completion.
commandAlias :: String -> Parser Command -> Mod CommandFields Command
commandAlias alias cmdParser = command alias . info cmdParser $ mempty

-- | Helper for building a parser for a group of commands.
commandsParser :: String -> String -> [(String, [String], Parser Command, String)] -> Parser Command
commandsParser mv groupName commands =
  hsubparser (metavar mv <> mconcat (fmap
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
analysesParser = commandsParser "ANALYSIS_COMMAND" "Analysis Commands" analysesCommands
  where
    analysesCommands =
      [ ("count",
          [],
          cmdCount,         "count variable declarations")
      , ("implicit-none",
          [],
          cmdImplicitNone,  "check 'implicit none' completeness")
      , ("implicit-none-all",
          [],
          cmdImplicitNoneAll,  "check 'implicit none' completeness (all program units)")
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
      , ("units-summarise",
          ["unit-compile", "units-summarize", "units-compile", "compile-units", "compile-unit"],
          cmdUnitsCompile,    "unit-of-measure summarisation")
      , ("units-synth",
          ["unit-synth", "synth-units", "synth-unit"],
          cmdUnitsSynth,    "unit-of-measure synthesise specs")
      , ("invariants-check",
          ["invariants-check","check-invariants", "check-hoare", "hoare-check"],
          cmdInvariantsCheck, "hoare logic invariant checking")
      , ("alloc-check",
          [],
          cmdAllocCheck,  "check allocate/deallocate statement usage")
      , ("fp-check",
          [],
          cmdFPCheck, "check floating point usage")
      , ("use-check",
          [],
          cmdUseCheck, "check usage of module USE")
      , ("array-check",
          [],
          cmdArrayCheck, "check usage of arrays")
      , ("basic-checks",
          [],
          cmdBasicChecks, "run a series of basic checks: alloc, array, fp, implicit-none, use")
      ]


refactoringsParser :: Parser Command
refactoringsParser = commandsParser "REFACTORING_COMMAND" "Refactoring Commands" refactoringsCommands
  where
    refactoringsCommands =
      [ ("common",            [],      cmdRefactCommon,      "common block elimination")
      , ("equivalence",       [],      cmdRefactEquivalence, "equivalence elimination")
      , ("dead",              [],      cmdRefactDead,        "dead-code elimination") ]

derivedDatatypeParser :: Parser Command
derivedDatatypeParser = commandsParser "DDT_COMMAND" "Derived Datatype Commands" derivedDatatypeCommands
  where
    derivedDatatypeCommands =
      [ ("ddt-infer",    ["infer-ddt"],    cmdInferDDT,   "infer derived datatypes")
      , ("ddt-synth",    ["synth-ddt"],    cmdSynthDDT,   "synthesise comments for derived datatypes")
      , ("ddt-check",    ["check-ddt"],    cmdCheckDDT,   "check comments for derived datatypes")
      , ("ddt-refactor", ["refactor-ddt"], cmdRefactDDT,  "refactor marked derived datatypes from comments")
      , ("ddt-compile",  ["compile-ddt"],  cmdCompileDDT, "compile derived datatypes info")]

showVersionParser :: Parser Command
showVersionParser = flag' CmdTopVersion $  long "version"
                                        <> short 'v'
                                        <> short '?'
                                        <> help "show version number"

cmdInit :: FilePath -> Parser Command
cmdInit currDir = fmap CmdInit . directoryArgument $
  help "project directory" <> value currDir

projectParser :: FilePath -> Parser Command
projectParser currDir = commandsParser "PROJECT_COMMAND" "Project Commands" projectCommands
  where
    projectCommands =
      [ ("init", [], cmdInit currDir, "initialize CamFort for the project") ]

-- | Collective parser for all CamFort commands.
commandParser :: FilePath -> Parser Command
commandParser currDir =
  helper <*> (    projectParser currDir
              <|> analysesParser
              <|> refactoringsParser
              <|> derivedDatatypeParser
              <|> showVersionParser )


-- | Current CamFort version.
version :: String
version = "1.2.0"


-- | Full CamFort version string.
versionMessage :: String
versionMessage = "CamFort " ++ version ++ " - Cambridge Fortran Infrastructure."


-- | Print the full version string.
displayVersion :: IO ()
displayVersion = putStrLn versionMessage
