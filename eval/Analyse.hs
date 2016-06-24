import Text.Regex.PCRE ((=~))
import System.Environment
import qualified Data.ByteString.Char8 as S
import Data.Maybe (fromMaybe)
import Data.List (unfoldr, groupBy, foldl')
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Map as M

keywords = [ "readOnce", "reflexive", "irreflexive", "forward", "backward", "centered"
           , "atMost" , "atLeast", "boundedBoth", "nonNeighbour", "emptySpec"
           , "inconsistentIV", "tickAssign", "LHSnotHandled" ]

type Analysis = M.Map String Int
type ModAnalysis = M.Map String Analysis

-- Count interesting stuff in a given line, and given the group's
-- analysis to this point.
countKeys :: Analysis -> S.ByteString -> Analysis
countKeys a l
  | get a "atLeast" > 0 && get curA "atMost" > 0 ||
    get curA "atLeast" > 0 && get a "atMost" > 0 = M.insertWith (+) "boundedBoth" n a'
  | otherwise                                    = a'
  where
    a'      = a `M.union` curA
    curA    = M.fromList [ (k, if l =~ re k then n else 0) | k <- keywords ]
    re k    = "[^A-Za-z]" ++ k ++ "[^A-Za-z]"
    get a k = 0 `fromMaybe` M.lookup k a
    n       = numVars l

-- Group by source span and variable in order to group multiple lines
-- that happen to pertain to the same expression (e.g. for boundedBoth).
analyseExec :: [S.ByteString] -> ModAnalysis
analyseExec [] = M.empty
analyseExec (e1:es) = M.singleton modName . M.unionsWith (+) . map eachGroup $ gs
  where
    gs = groupBy srcSpanAndVars . filter (=~ "\\)[[:space:]]*(stencil|EVALMODE)") $ es
    modName = drop 4 . S.unpack . (=~ "MOD=([^ ]*)") $ e1

-- helper functions
srcSpan :: S.ByteString -> S.ByteString
srcSpan = (=~ "^\\([^[:space]]*\\)")
vars :: S.ByteString -> S.ByteString
vars = (=~ ":: .*$")
srcSpanAndVars l1 l2 = srcSpan l1 == srcSpan l2 && vars l1 == vars l2
numVars = (1 +) . S.length . S.filter (== ',') . vars
eachGroup :: [S.ByteString] -> Analysis
eachGroup = foldl' countKeys M.empty

-- Extract one set of text that occurs between "%%% begin" and "%%% end",
-- returning the remainder if present.
oneExec :: [S.ByteString] -> Maybe ([S.ByteString], [S.ByteString])
oneExec ls
  | null b    = Nothing
  | otherwise = Just (e, b)
  where
    a = dropWhile (not . (=~ "^%%% begin stencils-infer")) ls
    (e, b) = break (=~ "^%%% end stencils-infer") a

prettyOutput :: ModAnalysis -> String
prettyOutput = unlines . prettyOutput'
prettyOutput' :: ModAnalysis -> [String]
prettyOutput' ma = concat [ ("corpus module: " ++ m):map (replicate 4 ' ' ++) (prettyAnal a) | (m, a) <- M.toList ma ] ++
                          ( "overall":map (replicate 4 ' ' ++) (prettyAnal combined) )
  where combined = M.unionsWith (+) $ M.elems ma
prettyAnal a = [ k ++ ": " ++ show v | (k, v) <- M.toList a ]

main :: IO ()
main = do
  input <- S.getContents
  let ls = S.lines input
  let execs = unfoldr oneExec ls
  let ma = M.unionsWith (M.unionWith (+)) $ map analyseExec execs
  args <- getArgs
  case args of
    []     -> putStrLn . prettyOutput $ ma
    "-R":_ -> print ma
  return ()

runTest = M.unionsWith (M.unionWith (+)) . map analyseExec . unfoldr oneExec

test1 = map S.pack [
      "%%% begin stencils-infer MOD=samples FILE=\"/home/mrd45/src/corpus/samples/weird/w1.f\""
    , "CamFort 0.775 - Cambridge Fortran Infrastructure."
    , "Inferring stencil specs for \"/home/mrd45/src/corpus/samples/weird/w1.f\""
    , ""
    , "Output of the analysis:"
    , "((38,9),(38,32))         stencil readOnce, reflexive(dims=1) :: real"
    , "((38,9),(38,32))         EVALMODE: Non-neighbour relative subscripts (tag: nonNeighbour)"
    , "((44,11),(44,23))        stencil readOnce, reflexive(dims=1) :: u"
    , "((51,12),(51,54))        stencil readOnce, reflexive(dims=1) :: real, v"
    , "((51,12),(51,54))        EVALMODE: Non-neighbour relative subscripts (tag: nonNeighbour)"
    , "((56,12),(56,22))        stencil readOnce, reflexive(dims=1) :: v"
    , "((70,9),(70,34))         stencil readOnce, irreflexive(dims=1), (backward(depth=1, dim=1)) :: c"
    , "((70,9),(70,34))         stencil readOnce, reflexive(dims=1) :: d"
    , "((71,9),(71,34))         stencil readOnce, (backward(depth=1, dim=1)) :: b"
    , "((75,9),(75,40))         stencil readOnce, reflexive(dims=1) :: b, c, d, e"
    , "((75,9),(75,40))         stencil readOnce, irreflexive(dims=1), (forward(depth=1, dim=1)) :: x"
    , ""
    , "0.14user 0.00system 0:00.15elapsed 98%CPU (0avgtext+0avgdata 20616maxresident)k"
    , "0inputs+8outputs (0major+1395minor)pagefaults 0swaps"
    , "%%% end stencils-infer MOD=samples FILE=\"/home/mrd45/src/corpus/samples/weird/w1.f\""
    , "%%% begin stencils-infer MOD=samples FILE=\"/home/mrd45/src/corpus/samples/weird/w1.f90\""
    , "CamFort 0.775 - Cambridge Fortran Infrastructure."
    , "Inferring stencil specs for \"/home/mrd45/src/corpus/samples/weird/w1.f90\""
    , ""
    , "Output of the analysis:"
    , ""
    , "0.10user 0.00system 0:00.11elapsed 96%CPU (0avgtext+0avgdata 20908maxresident)k"
    , "0inputs+8outputs (0major+1510minor)pagefaults 0swaps"
    , "%%% end stencils-infer MOD=samples FILE=\"/home/mrd45/src/corpus/samples/weird/w1.f90\""
    , "%%% begin stencils-infer MOD=um FILE=\"/home/mrd45/um/trunk/src/io_services/server/stash/ios_server_coupler.F90\""
    , "LineCount: 155"
    , "StartTime: 2016-06-24 11:23:44+01:00"
    , "CamFort 0.775 - Cambridge Fortran Infrastructure."
    , "Inferring stencil specs for \"/home/mrd45/um/trunk/src/io_services/server/stash/ios_server_coupler.F90\""
    , ""
    , "Output of the analysis:"
    , ""
    , "/home/mrd45/um/trunk/src/io_services/server/stash/ios_server_coupler.F90"
    , "((108,3),(108,60))       stencil readOnce, reflexive(dims=2) :: offset_map"
    , "((108,3),(108,60))       EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((109,3),(110,37))       stencil readOnce, reflexive(dims=2) :: offset_map, size_map"
    , "((109,3),(110,37))       EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((111,3),(112,55))       stencil readOnce, reflexive(dims=1) :: grid_row_end, grid_row_start"
    , "((111,3),(112,55))       EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((130,3),(130,41))       stencil reflexive(dims=2) :: size_map"
    , "((130,3),(130,41))       EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((136,3),(136,37))       stencil reflexive(dims=2) :: size_map"
    , "((136,3),(136,37))       EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((137,3),(138,59))       stencil readOnce, reflexive(dims=1) :: grid_point_end, grid_point_start"
    , "((137,3),(138,59))       EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , ""
    , "1.33user 0.04system 0:02.08elapsed 66%CPU (0avgtext+0avgdata 75672maxresident)k"
    , "0inputs+0outputs (0major+15984minor)pagefaults 0swaps"
    , "EndTime: 2016-06-24 11:23:46+01:00"
    , "%%% end stencils-infer MOD=um FILE=\"/home/mrd45/um/trunk/src/io_services/server/stash/ios_server_coupler.F90\""
  ]

-- Local variables:
-- mode: haskell
-- haskell-program-name: "stack repl camfort:exe:analyse"
-- End:
