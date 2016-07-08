import Text.Printf (printf)
import Text.Regex.PCRE ((=~))
import Text.Regex.Base
import System.Environment
import qualified Data.ByteString.Char8 as S
import Data.Maybe
import Data.List (unfoldr, groupBy, foldl', nub)
import Data.Function (on)
import Data.Array
import qualified Data.Map as M
import Control.Monad

varKeywords = [ "readOnce", "reflexive", "irreflexive", "forward", "backward", "centered", "atLeast", "atMost" ]
spanKeywords = [ "tickAssign", "LHSnotHandled", "nonNeighbour", "emptySpec", "inconsistentIV", "relativized" ]

dimName :: Int -> String
dimName n = "dims"++show n
depthName :: Int -> String
depthName n = "depth"++show n
regionOpsName :: Int -> String
regionOpsName n = "regionOps"++ show n
plusOpsName :: Int -> String
plusOpsName n = "plusOps" ++ show n
mulOpsName :: Int -> String
mulOpsName n = "mulOps" ++ show n
dimDistName :: Int -> String
dimDistName n = "dimDist" ++ show n
dimTagName :: Int -> String
dimTagName n = "dimTag" ++ show n

type Analysis = M.Map String Int
type ModAnalysis = M.Map String Analysis

-- Alongside countBySpan, also do some counts grouped by spans and variables.
countByVars :: Analysis -> S.ByteString -> Analysis
countByVars a l = M.unionsWith (+) $ [a, keysA] ++ map snd (filter fst counts)
  where
    counts  = [ ((get a "atLeast" > 0 && get keysA "atMost" > 0) ||
                (get keysA "atLeast" > 0 && get a "atMost" > 0),   {- ==> -} M.singleton "boundedBoth" n)
              , ( get keysA "reflexive" > 0 &&
                  M.size keysA == 1                            ,   {- ==> -} M.singleton "justReflexive" n )
              , ( ndims > 0                                    ,   {- ==> -} M.singleton (dimName ndims) n)
              , ( isJust mdep                                  ,   {- ==> -} M.singleton (depthName (fromJust mdep)) n)
              , ( regOps >= 0 && isSten                        ,   {- ==> -} M.singleton (regionOpsName regOps) n)
              , ( plusOps >= 0 && isSten                       ,   {- ==> -} M.singleton (plusOpsName plusOps) n)
              , ( mulOps >= 0 && isSten                        ,   {- ==> -} M.singleton (mulOpsName mulOps) n)
              , ( dimDist > 0 && isSten                        ,   {- ==> -} M.singleton (dimDistName dimDist) n)
              ]
    keysA   = M.fromList [ (k, n) | k <- varKeywords, l =~ re k  ] -- try each keyword
    re k    = "[^A-Za-z]" ++ k ++ "[^A-Za-z]" -- form a regular expression from a keyword
    get m k = 0 `fromMaybe` M.lookup k m -- convenience function
    n       = numVars l -- note that this will treat an EVALMODE line as having 1 variable
    ndims   = numDims l
    mdep    = depth l
    regOps  = countRegionOps l
    plusOps = countPlusOps l
    mulOps  = countMulOps l
    dimDist = countDimDist l
    isSten  = l =~ "\\)[[:space:]]*stencil "

-- Count interesting stuff in a given line, and given the group's
-- analysis to this point, grouped by span.
countBySpan :: Analysis -> S.ByteString -> Analysis
countBySpan a l = M.unionsWith (+) $ [a, keysA] ++ map snd (filter fst counts)
  where
    counts  = [ ( l =~ "stencil "                , {- ==> -} M.fromList [("numStencilLines", 1),
                                                                         ("numStencilSpecs", n)] )
              , ( get a "numStencilSpecs" > 0 &&
                  get keysA "tickAssign" > 0     , {- ==> -} M.singleton "tickAssignSuccess" 1 )
              , ( isJust mDimTag                 , {- ==> -} M.singleton (dimTagName (fromJust mDimTag)) n)
              ]
    keysA   = M.fromList [ (k, n) | k <- spanKeywords, l =~ re k  ] -- try each keyword
    re k    = "[^A-Za-z]" ++ k ++ "[^A-Za-z]" -- form a regular expression from a keyword
    get m k = 0 `fromMaybe` M.lookup k m -- convenience function
    n       = numVars l -- note that this will treat an EVALMODE line as having 1 variable
    mDimTag = countDimTag l

-- Look at each group that shares a source span, then each group that shares a source span and vars.
eachGroup :: [S.ByteString] -> Analysis
eachGroup g = M.unionsWith (+) $ a':map (foldl' countByVars M.empty) gbv
  where
    a'  = foldl' countBySpan M.empty g
    gbv = groupBy ((==) `on` vars) (filter (not . S.null . vars) g)

-- Group by source span and variable in order to group multiple lines
-- that happen to pertain to the same expression (e.g. for boundedBoth).
analyseExec :: [S.ByteString] -> ModAnalysis
analyseExec [] = M.empty
analyseExec (e1:es)
  | any (=~ "Lexing failed") es = M.singleton modName . M.fromList $ [("lexFailed", 1), ("lexOrParseFailed", 1)] ++ linesTotal
  | any (=~ "Parsing failed") es = M.singleton modName . M.fromList $ [("parseFailed", 1), ("lexOrParseFailed", 1)] ++ linesTotal
  | otherwise = M.singleton modName . M.unionsWith (+) . (parseOk:) . map eachGroup $ gs
  where
    gs         = groupBy ((==) `on` srcSpan) . filter (=~ "\\)[[:space:]]*(stencil|EVALMODE)") $ es
    modName    = drop 4 . S.unpack . (=~ "MOD=([^ ]*)") $ e1
    lineCount  = msum (es >>= map (fmap fst . S.readInt) . mrSubList . (=~ "LineCount: ([0-9]*)"))
    linesTotal = case lineCount of Just n -> [("linesTotal", n)]; _ -> []
    parseOk    = M.fromList (("parseOk", 1):(case lineCount of Just n -> [("linesParsed", n)]; _ -> []) ++ linesTotal)

-- helper functions
srcSpan :: S.ByteString -> S.ByteString
srcSpan = (=~ "^\\([^[:space:]]*\\)")
vars :: S.ByteString -> S.ByteString
vars = (=~ ":: .*$")
srcSpanAndVars l1 l2 = srcSpan l1 == srcSpan l2 && vars l1 == vars l2
numVars = (1 +) . S.length . S.filter (== ',') . vars
numDims :: S.ByteString -> Int
numDims s
  | S.null match = 0
  | otherwise  = (1 +) . S.length . S.filter (== ',') $ match
  where
    match = s =~ "\\(dims=[^\\)]*\\)"
depth :: S.ByteString -> Maybe Int
depth = fmap fst . S.readInt . S.concat . mrSubList . (=~ "depth=([0-9]*)")
countRegionOps :: S.ByteString -> Int
countRegionOps = S.length . S.filter (`elem` "+*")
countMulOps :: S.ByteString -> Int
countMulOps = S.length . S.filter (`elem` "*")
countPlusOps :: S.ByteString -> Int
countPlusOps = S.length . S.filter (`elem` "+")
countDimDist :: S.ByteString -> Int
countDimDist l = length . nub . filter (>0) $ (dimList ++ dimsList)
  where
    matchToInt = fromMaybe 0 . fmap fst . S.readInt
    -- search for "dim=n"
    dimList    = map (matchToInt . (! 1)) (getAllTextMatches (l =~ "dim=([0-9]*)" :: AllTextMatches [] (Array Int S.ByteString)))
    -- search for "(dims=n,n,...)"
    dimsList   = map matchToInt . concatMap (S.split ',') . mrSubList $ l =~ "\\(dims=([^\\)]*)\\)"
countDimTag :: S.ByteString -> Maybe Int
countDimTag = fmap fst . S.readInt . S.concat . mrSubList . (=~ "dimensionality=([0-9]*)")

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
                          ( "overall":map (replicate 4 ' ' ++) (prettyAnal combined) ) ++
                          ( map (replicate 4 ' '++) [
                              printf "numStencilSpecs (%d) <= forward (%d) + backward (%d) + centered (%d) + reflexive (%d) = %d"
                                     numStencilSpecs forward backward centered reflexive numSpeced
                              ] )
  where
    combined = M.unionsWith (+) $ M.elems ma
    get = fromMaybe 0 . flip M.lookup combined
    numStencilSpecs = get "numStencilSpecs"
    forward = get "forward"
    backward = get "backward"
    centered = get "centered"
    reflexive = get "reflexive"
    numSpeced = forward + backward + centered + reflexive
prettyAnal a = [ k ++ ": " ++ show v | (k, v) <- M.toList a ]


main :: IO ()
main = do
  input <- S.getContents
  let ls = S.lines input
  let execs = unfoldr oneExec ls
  let ma = M.unionsWith (M.unionWith (+)) $ map analyseExec execs
  args <- getArgs
  case args of
    "-R":_ -> print ma
    _      -> putStrLn . prettyOutput $ ma
  return ()

runTest = M.unionsWith (M.unionWith (+)) . map analyseExec . unfoldr oneExec

test0 = map S.pack [
      "%%% begin stencils-infer MOD=samples FILE=\"/home/mrd45/src/corpus/samples/weird/w1.f\""
    , "CamFort 0.775 - Cambridge Fortran Infrastructure."
    , "Inferring stencil specs for \"/home/mrd45/src/corpus/samples/weird/w1.f\""
    , ""
    , "Output of the analysis:"
    , "((38,9),(38,32))         stencil readOnce, reflexive(dims=1) :: real"
    , "((38,9),(38,32))         EVALMODE: Non-neighbour relative subscripts (tag: nonNeighbour)"
    , "((42,9),(42,32))         stencil readOnce, reflexive :: x"
    , "%%% end stencils-infer MOD=samples FILE=\"/home/mrd45/src/corpus/samples/weird/w1.f\""
    ]

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
    , "((139,24),(141,48))      stencil readOnce, irreflexive(dims=1,2,3), (reflexive(dim=1))*(centered(depth=1, dim=2)) + (reflexive(dim=2))*(centered(depth=1, dim=1)) :: p"
    , "((139,24),(141,48))      stencil readOnce, (reflexive(dim=1))*(reflexive(dim=2)) :: rhs"
    , "((147,7),(147,78))       EVALMODE: LHS is an array subscript we  can't handle (tag: LHSnotHandled)"
    , "((148,10),(148,68))      EVALMODE: dimensionality=1"
    , ""
    , "1.33user 0.04system 0:02.08elapsed 66%CPU (0avgtext+0avgdata 75672maxresident)k"
    , "0inputs+0outputs (0major+15984minor)pagefaults 0swaps"
    , "EndTime: 2016-06-24 11:23:46+01:00"
    , "%%% end stencils-infer MOD=um FILE=\"/home/mrd45/um/trunk/src/io_services/server/stash/ios_server_coupler.F90\""
  ]

test2 = map S.pack [
      "%%% begin stencils-infer MOD=um FILE=\"/home/mrd45/um/trunk/src/scm/initialise/initqlcf.F90\""
    , "LineCount: 210"
    , "StartTime: 2016-06-24 12:28:13+01:00"
    , "Progress: 46 / 2533"
    , "CamFort 0.775 - Cambridge Fortran Infrastructure."
    , "Inferring stencil specs for \"/home/mrd45/um/trunk/src/scm/initialise/initqlcf.F90\""
    , ""
    , "Output of the analysis:"
    , ""
    , "/home/mrd45/um/trunk/src/scm/initialise/initqlcf.F90"
    , "((126,5),(126,18))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((131,9),(131,66))      stencil readOnce, reflexive(dims=1,2) :: q, wqsat"
    , "((131,9),(131,66))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((132,9),(132,36))      stencil reflexive(dims=1,2) :: ocf"
    , "((132,9),(132,36))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((136,9),(136,42))      stencil readOnce, reflexive(dims=1,2) :: q, wqsat"
    , "((136,9),(136,42))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((137,9),(137,42))      stencil readOnce, reflexive(dims=1,2) :: ocf"
    , "((137,9),(137,42))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((146,9),(146,22))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((148,9),(148,22))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((195,5),(195,19))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((196,5),(196,19))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((199,7),(199,20))      stencil reflexive(dims=1,2) :: ocf"
    , "((199,7),(199,20))      stencil readOnce, reflexive(dims=1,2) :: q"
    , "((199,7),(199,20))      stencil reflexive(dims=1,2) :: t, wqsat"
    , "((199,7),(199,20))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((201,7),(201,20))      stencil reflexive(dims=1,2) :: ocf"
    , "((201,7),(201,20))      stencil readOnce, reflexive(dims=1,2) :: q"
    , "((201,7),(201,20))      stencil reflexive(dims=1,2) :: t, wqsat"
    , "((201,7),(201,20))      EVALMODE: assign to relative array subscript (tag: tickAssign)"
    , "((275,11),(275,54))     stencil atLeast, readOnce, reflexive(dims=1,2,3) :: p"
    , "((275,11),(275,54))     stencil atMost, readOnce, (forward(depth=2, dim=3)) :: p"
    , ""
    , "1.23user 0.03system 0:01.83elapsed 69%CPU (0avgtext+0avgdata 76092maxresident)k"
    , "0inputs+0outputs (0major+15975minor)pagefaults 0swaps"
    , "EndTime: 2016-06-24 12:28:15+01:00"
    , "%%% end stencils-infer MOD=um FILE=\"/home/mrd45/um/trunk/src/scm/initialise/initqlcf.F90\""
  ]

test2out = M.fromList [("um",M.fromList [("atLeast",1),("atMost",1),("boundedBoth",1),("depth2",1),("dims2",14),("dims3",1),("forward",1),("justReflexive",7),("numStencilLines",12),("numStencilSpecs",16),("parseOk",1),("readOnce",9),("reflexive",15),("tickAssign",11),("tickAssignSuccess",6)])]

runTests = do
  print $ runTest test2 == test2out

-- Local variables:
-- mode: haskell
-- End:
