import Data.List

data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq)

-- Give more meaningful names.
type Notes = [SingleNote]
type Steps = [Int]

-- There are 0..24 Frets on this guitar.
numFrets :: Int
numFrets = 6

-- Gets the successor of the SingleNote passed in. Wraps around so that
-- the datatype SingleNote is circular.
halfStep :: SingleNote -> SingleNote
halfStep Ab = A
halfStep n = succ n

-- Returns a SingleNote that is `n` steps from the SingleNote passed in.
nSteps :: SingleNote -> Int -> SingleNote
nSteps n x = iterate halfStep n !! x

-- ====== SCALES =================================
-- Generates a scale of SingleNotes, using the SingleNote argument
-- as the starting point and the a predefined array of int steps as the scale.
genScale :: SingleNote -> Steps -> Notes
genScale n ss = [nSteps n s | s <- ss]

ionian, majTriad, minTriad, maj7th, min7th, sixthAdd9th, sus4th :: SingleNote -> Notes
ionian root      = genScale root [0, 2, 4, 5, 7, 9, 11, 12]
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
chromaticScale maxFret root = take maxFret (iterate halfStep root)

-- ===============================================

-- Generates list of indices where the notes in the scale are located on the
-- fretboard for a given string tuned to a specific note.
positions :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions maxFret tuning scale root = [getPos (scale root) | t <- tuning,
    let fretPosition s = elemIndices s (chromaticScale (maxFret+1) t),
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
putNotes :: Notes -> String
putNotes [n] = show n
putNotes (n:ns) = show n ++ " " ++ putNotes ns

putSteps :: Steps -> String
putSteps [] = "x"
putSteps [n] = show n
putSteps (n:ns) = show n ++ " " ++ putSteps ns

-- =========================================================
-- Build Tab Diagram
buildTabStrings :: [(SingleNote, Steps)] -> [String]
buildTabStrings xs = [formatTuning (fst x) ++ "||" ++ showPositions (snd x) | x <- xs]
    where showPositions [] = "--x"
          showPositions [x] = if x < 10 then "- " ++ show x
                              else "-" ++ show x
          showPositions (x:xs) = showPositions [x] ++ showPositions xs
          formatTuning n = if 'b' `elem` show n  then show n ++ " "
                           else show n ++ "  "

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
buildFretStrings :: Int -> [(SingleNote, Steps)] -> [String]
buildFretStrings maxFret nt =
    [formatTuning (fst ns) ++ concat (makeFrets maxFret (snd ns)) | ns <- nt]

makeFrets maxFret [] = ["|x|" ++ concat (replicate maxFret "-----|")]
makeFrets maxFret (n:ns) =
    if n == 0 then return ("|o|" ++ makeStrings maxFret ns)
    else return ("| |" ++ makeStrings maxFret (n:ns))

makeStrings maxFret ns = do
    fret <- [1..maxFret]
    if fret `elem` ns then "--o--|"
    else "-----|"

formatTuning n = if 'b' `elem` show n then show n ++ " "
     else show n ++ "  "

buildFrets :: Args -> Int -> [Steps] -> [String]
buildFrets args@(Args tuning scale root fret) maxFret ns = do
    let nt = zip tuning ns
    buildFretStrings maxFret nt

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


--    putStrLn ("     0   " ++ intercalate "     " (map show [1..maxFrets]))

-- Functions to output guitar things.
makeGuitar :: Int -> Notes -> IO()
makeGuitar maxFret tuning = putStrLn $ unlines $ map putNotes (allNotes tuning)
            where allNotes tuning = [chromaticScale maxFret n | n <- tuning]

makeChordTab :: Args -> IO()
makeChordTab args@(Args tuning scale root fret) = do
    let maxFret = fret + 4
    let ns = fingering1 args maxFret
    fretHeader root fret >> mapM_ putStrLn (buildTab args maxFret ns)

makeChordAll :: Args -> Int -> IO()
makeChordAll args@(Args tuning scale root fret) maxFret = do
    let ns = fingering args maxFret
    fretHeader root fret >> mapM_ putStrLn (buildFrets args maxFret ns) >> fretFooter maxFret

makeChordOne :: Args -> Int -> IO()
makeChordOne args maxFret = do
    let ns = fingering1 args maxFret
    fretHeader (root args)(fret args)
        >> mapM_ putStrLn (buildFrets args maxFret ns) >> fretFooter maxFret

-- take a min and max value for the fret display.
-- maybe a UI to choose fret or tab displays?
-- maybe different data type for fret/tab?
-- figure out using `take` for fret/tab. (Tab takes 1, Fret it doesn't matter)
