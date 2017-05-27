import Data.List

data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq)

keysWithSharps :: Notes
keysWithSharps = [A, B, D, E, G]

-- Give more meaningful names.
type Notes = [SingleNote]
type Steps = [Int]

-- Gets the successor of the SingleNote passed in. Wraps around so that
-- the datatype SingleNote is circular.
halfStepU :: SingleNote -> SingleNote
halfStepU Ab = A
halfStepU n = succ n

halfStepD :: SingleNote -> SingleNote
halfStepD A = Ab
halfStepD n = pred n

-- Returns a SingleNote that is `n` steps from the SingleNote passed in.
nSteps :: SingleNote -> Int -> SingleNote
nSteps n x = iterate halfStepU n !! x

-- ====== SCALES =================================
-- Generates a scale of SingleNotes, using the SingleNote argument
-- as the starting point and the a predefined array of int steps as the scale.
genScale :: SingleNote -> Steps -> Notes
genScale n ss = [nSteps n s | s <- ss]

diatonic, pentatonic, majTriad, minTriad, maj7th, min7th, sixthAdd9th, sus4th :: SingleNote -> Notes
diatonic root      = genScale root [0, 2, 4, 5, 7, 9, 11, 12]
pentatonic root  = genScale root [0, 2, 4, 7, 9, 12]

majTriad root    = genScale root [7, 4, 0]
minTriad root    = genScale root [7, 3, 0]
maj7th root      = genScale root [10, 7, 4, 0]
min7th root      = genScale root [10, 7, 3, 0]
sixthAdd9th root = genScale root [14, 10, 7, 4, 0]
sus4th root      = genScale root [7, 5, 0]

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
-- NOTE: Maybe add nub to this? Doesn't seem to matter with the current algorithm.
fingering :: Args -> Int -> [Steps]
fingering (Args tuning scale root fret) maxFret =
    map (sort . filter (fret<=)) $ positions maxFret tuning scale root

fingering1 :: Args -> Int -> [Steps]
fingering1 args maxFret =
    map (take 1) $ fingering args maxFret

-- ===============================================

-- Functions for Output
-- ===============================================
flatSharp :: SingleNote -> String
flatSharp n = if 'b' `elem` (show n) then (head (show (halfStepD n)) : "#")
              else (show n) ++ "-"

putNotes :: Notes -> String
putNotes [] = "x"
putNotes ns = unwords $ map show ns

putSteps :: Steps -> String
putSteps [] = "x"
putSteps ns = unwords $ map show ns

-- =========================================================
-- Build Tab Diagram
buildTabStrings :: [(SingleNote, Steps)] -> [String]
buildTabStrings xs = [formatNote (fst x) ++ "||" ++ showPositions (snd x) | x <- xs]
    where showPositions [] = "--x"
          showPositions [x] = if x < 10 then "- " ++ show x
                              else "-" ++ show x
          showPositions (x:xs) = showPositions [x] ++ showPositions xs

formatNote n = if 'b' `elem` show n  then show n
                 else show n ++ " "

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

-- =========================================================
-- Build Fret Diagram
buildFretStrings :: SingleNote -> Int -> [(SingleNote, Steps)] -> [String]
buildFretStrings root maxFret nt =
    [formatTuning (fst ns) ++ concat (makeFrets (fst ns) root maxFret (snd ns)) | ns <- nt]

makeFrets :: SingleNote -> SingleNote -> Int -> Steps -> [String]
makeFrets str root maxFret [] = ["|x|" ++ concat (replicate maxFret "-----|")]
makeFrets str root maxFret (n:ns) =
    if n == 0 then return ("|o|" ++ makeStringNotes str root maxFret ns)
    else return ("| |" ++ makeStringNotes str root maxFret (n:ns))

makeStrings maxFret ns = do
    fret <- [1..maxFret]
    if fret `elem` ns then "--o--|"
    else "-----|"

makeStringNotes str root maxFret ns = do
    fret <- [1..maxFret]
    if fret `elem` ns then
        if root `elem` keysWithSharps then ("--" ++ flatSharp (nSteps str fret) ++ "-|")
        else ("--" ++ formatNote (nSteps str fret) ++ "-|")
    else "-----|"

formatTuning n = if 'b' `elem` show n then show n ++ " "
     else show n ++ "  "

buildFrets :: Args -> Int -> [Steps] -> [String]
buildFrets args@(Args tuning scale root fret) maxFret ns = do
    let nt = zip tuning ns
    buildFretStrings root maxFret nt

-- =========================================================
-- Data type to pass common args around as one unit.
data Args = Args { tuning :: Notes,
                   scale :: SingleNote -> Notes,
                   root :: SingleNote,
                   fret :: Int
                 }

fretHeader :: SingleNote -> Int -> IO()
fretHeader root fret = putStrLn ("\nChord: " ++ show root ++ "\nMin. Fret: " ++ show fret)

fretFooter :: Int -> IO()
fretFooter maxFret = do
    let prt = partition (10>) [1..maxFret]
    putStrLn ("    0   " ++ intercalate "     " (map show (fst prt))
             ++ "     " ++ intercalate "    " (map show (snd prt)))

-- Functions to output guitar things.
makeGuitar :: Int -> Notes -> IO()
makeGuitar maxFret tuning = putStrLn $ unlines $ map putNotes allNotes
            where allNotes = [chromaticScale maxFret n | n <- tuning]

makeChordTab :: Args -> IO()
makeChordTab args@(Args tuning scale root fret) = do
    let maxFret = fret + 4
    let ns = fingering1 args maxFret
    fretHeader root fret >> mapM_ putStrLn (buildTab args maxFret ns)

makeChordAll :: Args -> Int -> IO()
makeChordAll args@(Args tuning scale root fret) maxFret = do
    let ns = fingering args maxFret
    fretHeader root fret >> mapM_ putStrLn (buildFrets args maxFret ns) >> fretFooter maxFret

makeChordOne :: Args -> IO()
makeChordOne args = do
    let maxFret = fret args + 4
    let ns = fingering1 args maxFret
    fretHeader (root args)(fret args)
        >> mapM_ putStrLn (buildFrets args maxFret ns) >> fretFooter maxFret

-- take a min and max value for the fret display.
-- Factor out `fret` from data Args, pass where needed.

-- maybe a UI to choose fret or tab displays?
