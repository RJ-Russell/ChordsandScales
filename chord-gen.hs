import Data.List

-- Representation of notes.
data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq)

-- If key is in this list, then convert flats to sharps.
keysWithSharps :: Notes
keysWithSharps = [A, B, D, E, G]

-- Give more meaningful names.
type Notes = [SingleNote]
type Steps = [Int]

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
nSteps n x = iterate halfStepU n !! x

-- ====== SCALES =================================
-- Generates a scale of SingleNotes, using the SingleNote argument
-- as the starting point and the a predefined array of int steps as the scale.
genScale :: SingleNote -> Steps -> Notes
genScale n ss = [nSteps n s | s <- ss]

-- Scales.
diatonic, pentatonic :: SingleNote -> Notes
diatonic root      = genScale root [0, 2, 4, 5, 7, 9, 11, 12]
pentatonic root  = genScale root [0, 2, 4, 7, 9, 12]

-- Scales for chord construction.
majTriad, minTriad, maj7th, min7th, sixthAdd9th, sus4, sus2 :: SingleNote -> Notes
majTriad root = genScale root [0, 4, 7]
minTriad root = genScale root [0, 3, 7]
maj7th root = genScale root [0, 4, 7, 10]
min7th root = genScale root [0, 3, 7, 10]
sixthAdd9th root = genScale root [0, 4, 7, 10, 14]
sus4 root = genScale root [0, 5, 7]
sus2 root = genScale root [0, 1, 7]

-- ====== TUNINGS ================================
standard, dropD, halfDown, fullDown :: Notes
standard = [E, B, G, D, A, E]
dropD    = [E, B, G, D, A, D]
halfDown = [Eb, Bb, Gb, Db, Ab, Eb]
fullDown = [D, A, F, C, G, D]
openE    = [E, B, Ab, E, B, E]
ukulele  = [A, E, C, G]

-- Generates a chromatic scale using the SingleNote passed in as the starting point.
chromaticScale :: Int -> SingleNote -> Notes
chromaticScale maxFret root = take (maxFret + 1) (iterate halfStepU root)

-- ===============================================

-- Generates list of indices where the notes in the scale are located on the
-- fretboard for a given string tuned to a specific note.
positions :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions maxFret tuning scale root = [getPos (scale root) | t <- tuning,
    let fretPosition s = elemIndices s (chromaticScale maxFret t),
    let getPos ss = concat [fretPosition s | s <- ss]]

-- Generates a list of Steps for each note in the chord per string.
-- NOTE: Maybe add nub to this? Doesn't seem to matter at the moment.
fingering :: Args -> Int -> [Steps]
fingering (Args tuning scale root fret) maxFret =
    map (sort . filter (fret<=)) $ positions maxFret tuning scale root

fingering1 :: Args -> Int -> [Steps]
fingering1 args maxFret =
    map (take 1) $ fingering args maxFret

-- ===============================================
-- Functions for Output
-- ===============================================
putNotes :: Notes -> String
putNotes [] = "x"
putNotes ns = unwords $ map show ns

flatSharp :: SingleNote -> String
flatSharp n = if 'b' `elem` show n then head (show (halfStepD n)) : "#"
              else show n ++ "-"

formatTuning :: SingleNote -> String
formatTuning n = if 'b' `elem` show n then show n ++ " "
                 else show n ++ "  "

formatNote :: SingleNote -> String
formatNote n = if 'b' `elem` show n  then show n
               else show n ++ "-"

-- =========================================================
-- Build Tab Diagram
-- =========================================================
buildTab :: Args -> Int -> [Steps] -> [String]
buildTab args@(Args tuning scale root fret) maxFret ns = do
    let nt = zip tuning ns
    let bds = buildTabStrings nt
    let maxLen = maximum $ map length bds
    b <- bds
    return (b ++ genSpaces maxLen b ++ "-|")
    where
        genSpaces maxLen n
            | (maxLen - length n) == 0 = ""
            | otherwise = concat $ replicate (maxLen - length n) "-"

buildTabStrings :: [(SingleNote, Steps)] -> [String]
buildTabStrings xs = [formatTuning (fst x) ++ "||" ++ showPositions (snd x) | x <- xs]
    where showPositions [] = "--x"
          showPositions [x] = if x < 10 then "--" ++ show x
                              else "-" ++ show x
          showPositions (x:xs) = showPositions [x] ++ showPositions xs

-- =========================================================
-- Build Fret Diagram with Symbols
-- =========================================================
buildFretSymbols :: Args -> Int -> [Steps] -> [String]
buildFretSymbols args@(Args tuning scale root fret) maxFret ns = do
    let nt = zip tuning ns
    buildStringSymbols maxFret nt

buildStringSymbols :: Int -> [(SingleNote, Steps)] -> [String]
buildStringSymbols maxFret nt =
    [formatTuning (fst ns) ++ concat (fretSymbols maxFret (snd ns)) | ns <- nt]

fretSymbols :: Int -> Steps -> [String]
fretSymbols maxFret [] = ["|x|" ++ concat (replicate maxFret "-----|")]
fretSymbols maxFret (n:ns) =
    if n == 0 then return ("|o|" ++ makeStringSymbols maxFret ns)
    else return ("| |" ++ makeStringSymbols maxFret (n:ns))

makeStringSymbols :: Int -> Steps -> String
makeStringSymbols maxFret ns = do
    fret <- [1..maxFret]
    if fret `elem` ns then "--o--|"
    else "-----|"

-- =========================================================
-- Build Fret Diagram with Notes
-- =========================================================
buildFretNotes :: Args -> Int -> [Steps] -> [String]
buildFretNotes args@(Args tuning scale root fret) maxFret ns = do
    let nt = zip tuning ns
    buildStringNotes root maxFret nt

buildStringNotes :: SingleNote -> Int -> [(SingleNote, Steps)] -> [String]
buildStringNotes root maxFret nt =
    [formatTuning (fst ns) ++ concat (fretNotes (fst ns) root maxFret (snd ns)) | ns <- nt]

fretNotes :: SingleNote -> SingleNote -> Int -> Steps -> [String]
fretNotes str root maxFret [] = ["|x|" ++ concat (replicate maxFret "-----|")]
fretNotes str root maxFret (n:ns) =
    if n == 0 then return ("|o|" ++ makeStringNotes str root maxFret ns)
    else return ("| |" ++ makeStringNotes str root maxFret (n:ns))

makeStringNotes :: SingleNote -> SingleNote -> Int -> Steps -> String
makeStringNotes str root maxFret ns = do
    fret <- [1..maxFret]
    if fret `elem` ns then
        if root `elem` keysWithSharps then "--" ++ flatSharp (nSteps str fret) ++ "-|"
        else "--" ++ formatNote (nSteps str fret) ++ "-|"
    else "-----|"

-- =========================================================
-- Data type to pass common args around as one unit.
-- =========================================================
data Args = Args {
                   tuning :: Notes,
                   scale :: SingleNote -> Notes,
                   root :: SingleNote,
                   fret :: Int
                 }

-- =========================================================
-- Helper functions to display the header and footer of the diagrams.
-- =========================================================
fretHeader :: SingleNote -> Int -> String
fretHeader root fret = "\nRoot: " ++ show root ++ "\nMin. Fret: " ++ show fret

fretFooter :: Int -> [String]
fretFooter maxFret = do
    let prt = partition (10>) [1..maxFret]
    return ("    0   " ++ intercalate "     " (map show (fst prt))
             ++ "     " ++ intercalate "    " (map show (snd prt)))

-- =========================================================
-- Functions to output guitar things diagrams
-- =========================================================
-- Displays chromatic scale for each of the strings based on the tuning.
makeGuitar :: Notes -> IO()
makeGuitar tuning = do
            let maxFret = 12
            putStrLn $ "\n" ++ unlines (map putNotes (allNotes maxFret))
            where allNotes maxFret = [chromaticScale maxFret n | n <- tuning]

-- Displays a simple tab for a guitar chord with given parameters.
makeTab :: Args -> IO()
makeTab args@(Args tuning scale root fret) = do
    let maxFret = fret + 4
    let ns = fingering1 args maxFret
    putStrLn (fretHeader root fret) >> mapM_ putStrLn (buildTab args maxFret ns)

-- Displays all notes for a chord/scale, from the fret passed in to the 16th fret.
makeNotesAll :: Args -> IO()
makeNotesAll args@(Args tuning scale root fret) = do
    let maxFret = 16
    let ns = fingering args maxFret
    putStrLn (fretHeader root fret)
        >> mapM_ putStrLn (buildFretNotes args maxFret ns)
        >> putStrLn (unlines (fretFooter maxFret))

-- -- Displays notes for one position for a chord/scale based on the given parameters.
makeNotesOne :: Args -> IO()
makeNotesOne args@(Args tuning scale root fret) = do
    let maxFret = fret + 4
    let ns = fingering1 args maxFret
    putStrLn (fretHeader root fret)
        >> mapM_ putStrLn (buildFretNotes args maxFret ns)
        >> putStrLn (unlines (fretFooter maxFret))

-- -- Displays all symbols for all the positions of a chord/scale,
-- -- from the fret passed in to the 16th fret.
makeSymbolsAll :: Args -> IO()
makeSymbolsAll args@(Args tuning scale root fret) = do
    let maxFret = 16
    let ns = fingering args maxFret
    putStrLn (fretHeader root fret)
        >> mapM_ putStrLn (buildFretSymbols args maxFret ns)
        >> putStrLn (unlines (fretFooter maxFret))

-- Displays symbols for the fingering of one position for a chord/scale
-- based on the given parameters.
makeSymbolsOne :: Args -> IO()
makeSymbolsOne args@(Args tuning scale root fret) = do
    let maxFret = fret + 4
    let ns = fingering1 args maxFret
    putStrLn (fretHeader root fret)
        >> mapM_ putStrLn (buildFretSymbols args maxFret ns)
        >> putStrLn (unlines (fretFooter maxFret))

-- take a min and max value for the fret display.
-- Factor out `fret` from data Args, pass where needed.
-- maybe a UI to choose fret or tab displays?
