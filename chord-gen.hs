import Data.List
import Text.Read (readMaybe)

-- Representation of notes.
data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq)

-- Give more meaningful names.
type Notes = [SingleNote]
type Steps = [Int]

-- If key is in this list, then convert flats to sharps.
keysWithSharps :: Notes
keysWithSharps = [A, B, D, E, G]

-- Gets the predecessor of the SingleNote passed in. Wraps around so that
-- the datatype SingleNote is circular.
halfStepD :: SingleNote -> SingleNote
halfStepD A = Ab
halfStepD n = pred n

-- Gets the successor of the SingleNote passed in. Wraps around so that
-- the datatype SingleNote is circular.
halfStepU :: SingleNote -> SingleNote
halfStepU Ab = A
halfStepU n = succ n

-- Returns a SingleNote that is `n` steps from the SingleNote passed in.
nSteps :: SingleNote -> Int -> SingleNote
nSteps base numSteps = iterate halfStepU base !! numSteps

-- Generates a chromatic scale using the SingleNote passed in as the starting point.
chromatic :: SingleNote -> Int -> Notes
chromatic base maxFret = take (1 + maxFret) (iterate halfStepU base)

-- ====== SCALES =================================
-- Generates a scale of SingleNotes, using the SingleNote argument
-- as the starting point and the a predefined array of int steps as the scale.
genScale :: SingleNote -> Steps -> Notes
genScale root ss = [nSteps root s | s <- ss]

-- Scales for chord construction.
majTriad, minTriad, maj7th, min7th, sus4, sus2 :: SingleNote -> Notes
majTriad root = genScale root [0, 4, 7]
minTriad root = genScale root [0, 3, 7]
maj7th root = genScale root [0, 4, 7, 10]
min7th root = genScale root [0, 3, 7, 10]
sus4 root = genScale root [0, 5, 7]
sus2 root = genScale root [0, 1, 7]

-- Scales.
diatonicMaj, diatonicMin, pentatonicMaj, pentatonicMin :: SingleNote -> Notes
diatonicMaj root = genScale root [0, 2, 4, 5, 7, 9, 11, 12]
diatonicMin root = genScale root [0, 2, 3, 5, 7, 8, 10, 12]
pentatonicMaj root = genScale root [0, 2, 4, 7, 9, 12]
pentatonicMin root = genScale root [0, 3, 5, 7, 10, 12]

-- ====== TUNINGS ================================
standard, dropD, halfDown, fullDown, openE, sevenRock, ukulele:: Notes
standard = [E, B, G, D, A, E]
dropD    = [E, B, G, D, A, D]
halfDown = [Eb, Bb, Gb, Db, Ab, Eb]
fullDown = [D, A, F, C, G, D]
openE    = [E, B, Ab, E, B, E]
sevenRock = [E, B, G, D, A, E, B]
ukulele  = [A, E, C, G]

-- ===============================================

-- Generates a list of Steps for each note in the chord per string.
fingering :: Notes -> Notes -> Int -> Int -> [Steps]
fingering tuning scaleNotes minFret maxFret =
    map (sort . filter(minFret<=))
        [fretPosition scaleNotes | t <- tuning,
            let fretPosition ss =
                    concat [elemIndices s (chromatic t maxFret) | s <- ss]]

fingering2 :: Notes -> Notes -> Int -> Int -> [Steps]
fingering2 tuning scaleNotes minFret maxFret =
    map (filterDifference . take 2) $ fingering tuning scaleNotes minFret maxFret
    where
        filterDifference :: Steps -> Steps
        filterDifference [] = []
        filterDifference [x] = [x]
        filterDifference [x, y] | abs(x - y) <= 2 = [x, y]
                                | otherwise = [x]

-- ===============================================
-- Functions for Output
-- ===============================================
putNotes :: Notes -> String
putNotes [] = "x"
putNotes ns = unwords $ map show ns

flatSharp :: SingleNote -> String
flatSharp n = if 'b' `elem` show n then head (show (halfStepD n)) : "#"
              else show n ++ "-"

formatNote :: SingleNote -> String
formatNote n = if 'b' `elem` show n  then show n
               else show n ++ " "

formatNoteDash :: SingleNote -> String
formatNoteDash n = if 'b' `elem` show n  then show n
                   else show n ++ "-"
-- =========================================================
-- Build Tab Diagram
-- =========================================================
buildTab :: [(SingleNote, Steps)] -> [String]
buildTab nts = do
    let bts = buildTabStrings nts
    let maxLen = maximum $ map length bts
    b <- bts
    return (b ++ genSpaces maxLen b ++ "-|")
    where
        buildTabStrings :: [(SingleNote, Steps)] -> [String]
        buildTabStrings nts =
            [formatNote (fst nt) ++ "||" ++ showPositions (snd nt) | nt <- nts]

        showPositions :: Steps -> String
        showPositions [] = "--x"
        showPositions [s] = if s < 10 then "--" ++ show s
                            else "-" ++ show s
        showPositions (s:ss) = showPositions [s] ++ showPositions ss

        genSpaces :: Int -> String -> String
        genSpaces maxLen s
            | (maxLen - length s) == 0 = ""
            | otherwise = concat $ replicate (maxLen - length s) "-"

-- =========================================================
-- Build Fret Diagram with Symbols
-- =========================================================
buildFretSymbols :: [(SingleNote, Steps)] -> Int -> [String]
buildFretSymbols nts maxFret =
    [formatNote (fst nt) ++ fretSymbols maxFret (snd nt) | nt <- nts]
    where
        fretSymbols :: Int -> Steps -> String
        fretSymbols maxFret [] = "|x|" ++ concat (replicate maxFret "-----|")
        fretSymbols maxFret (n:ns)
            | n == 0 = "|o|" ++ makeStringSymbols maxFret ns
            | otherwise = "| |" ++ makeStringSymbols maxFret (n:ns)

        makeStringSymbols :: Int -> Steps -> String
        makeStringSymbols maxFret ns = do
            fret <- [1..maxFret]
            if fret `elem` ns then "--o--|"
            else "-----|"

-- =========================================================
-- Build Fret Diagram with Notes
-- =========================================================
buildFretNotes :: SingleNote -> [(SingleNote, Steps)] -> Int -> [String]
buildFretNotes root nts maxFret =
    [formatNote (fst nt) ++ fretNotes root nt maxFret | nt <- nts]
    where
        fretNotes :: SingleNote -> (SingleNote, Steps) -> Int -> String
        fretNotes _ (_, []) maxFret = "|x|" ++ concat (replicate maxFret "-----|")
        fretNotes root (base, n:ns) maxFret
            | n == 0 = "|o|" ++ makeStringNotes root base ns maxFret
            | otherwise = "| |" ++ makeStringNotes root base (n:ns) maxFret

        makeStringNotes :: SingleNote -> SingleNote -> Steps -> Int -> String
        makeStringNotes root base ns maxFret = do
            fret <- [1..maxFret]
            if fret `elem` ns then
                if root `elem` keysWithSharps then
                    "--" ++ flatSharp (nSteps base fret) ++ "-|"
                else "--" ++ formatNoteDash (nSteps base fret) ++ "-|"
            else "-----|"

-- =========================================================
-- Helper functions to displayBoard the header and footer of the diagrams.
-- =========================================================
fretHeader :: SingleNote -> Int -> String
fretHeader root minFret = "\nRoot: " ++ show root ++ "\nMin. Fret: " ++ show minFret

fretFooter :: Int -> String
fretFooter maxFret = do
    let prt = partition (10>) [1..maxFret]
    "   0   " ++ intercalate "     " (map show (fst prt))
     ++ "     " ++ intercalate "    " (map show (snd prt)) ++ "\n"

-- =========================================================
-- Functions to construct diagrams.
-- =========================================================
-- Generate chromatic scale for each of the strings based on the tuning.
buildGuitar :: Notes -> IO()
buildGuitar tuning = do
    clearScreen
    let maxFret = 12
    putStrLn $ "\n" ++ unlines (map putNotes (allNotes maxFret))
    where
        allNotes :: Int -> [Notes]
        allNotes maxFret = [chromatic n maxFret | n <- tuning]

-- Generate one position for a chord/scale.
buildSingle :: Notes -> (SingleNote -> Notes) -> SingleNote -> IO()
buildSingle tuning scale root = do
    clearScreen
    minFret <- getFret
    let maxFret = minFret + 4
        nt = zip tuning (fingering2 tuning (scale root) minFret maxFret)
        wrap = wrapBoard root minFret maxFret
    menu [("1", "Tab", mapM_ putStrLn (buildTab nt)),
          ("2", "Symbols", wrap(buildFretSymbols nt maxFret)),
          ("3", "Notes", wrap(buildFretNotes root nt maxFret))]

-- Generates all notes for a chord/scale, from fret 0 to the 16th fret.
buildAll :: Notes -> (SingleNote -> Notes) -> SingleNote -> IO()
buildAll tuning scale root = do
    clearScreen
    let minFret = 0
        maxFret = 16
        nt = zip tuning (fingering tuning (scale root) minFret maxFret)
        wrap = wrapBoard root minFret maxFret
    menu [("1", "Symbols", wrap (buildFretSymbols nt maxFret)),
          ("2", "Notes", wrap(buildFretNotes root nt maxFret))]

-- =========================================================
-- Helper functions for IO actions.
-- =========================================================
menu :: [(String, String, IO())] -> IO()
menu opts = do
    putStr . unlines $ map fopts opts
    putStr "Enter choice: "
    choice <- getLine
    case [cmd | (n, l, cmd) <- opts, n == choice] of
            [] -> do
                putStrLn "\nInvalid option...Try again!\n"
                menu opts
            (cmd:_) -> cmd
    where
        fopts (n, l, _) = n ++ ". " ++ l

getFret :: IO Int
getFret = do
    putStr "\nEnter minimum fret (0 - 12): "
    maybeFret <- getLine
    case readMaybe maybeFret of
        Just minFret | minFret >= 0 && minFret <= 12 -> return minFret
        _ -> do
            putStrLn "\nInvalid fret choice...Try again!"
            getFret

wrapBoard :: SingleNote -> Int -> Int -> [String] -> IO()
wrapBoard root minFret maxFret result =
    displayBoard([fretHeader root minFret] ++ result ++ [fretFooter maxFret])

displayBoard :: [String] -> IO()
displayBoard = mapM_ putStrLn

clearScreen :: IO()
clearScreen = putStr "\ESC[2J"
