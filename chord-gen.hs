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
majTriad, minTriad, maj7th, min7th, sus4, sus2 :: SingleNote -> Notes
majTriad root = genScale root [0, 4, 7]
minTriad root = genScale root [0, 3, 7]
maj7th root = genScale root [0, 4, 7, 10]
min7th root = genScale root [0, 3, 7, 10]
sus4 root = genScale root [0, 5, 7]
sus2 root = genScale root [0, 1, 7]

-- ====== TUNINGS ================================
standard, dropD, halfDown, fullDown :: Notes
standard = [E, B, G, D, A, E]
dropD    = [E, B, G, D, A, D]
halfDown = [Eb, Bb, Gb, Db, Ab, Eb]
fullDown = [D, A, F, C, G, D]
openE    = [E, B, Ab, E, B, E]
sevenRock = [E, B, G, D, A, E, B]
ukulele  = [A, E, C, G]

-- Generates a chromatic scale using the SingleNote passed in as the starting point.
chromaticScale :: Int -> SingleNote -> Notes
chromaticScale maxFret root = take (maxFret + 1) (iterate halfStepU root)

-- ===============================================

-- Generates a list of Steps for each note in the chord per string.
fingering :: Notes -> Notes -> Int -> Int -> [Steps]
fingering tuning scaleNotes fret maxFret =
        map (sort . filter(fret<=)) [getPos scaleNotes | t <- tuning,
            let fretPosition s = elemIndices s (chromaticScale maxFret t),
            let getPos ss = concat [fretPosition s | s <- ss]]

fingering2 :: Notes -> Notes -> Int -> Int -> [Steps]
fingering2 tuning scaleNotes fret maxFret =
    map (filterDifference . take 2) $ fingering tuning scaleNotes fret maxFret
    where
        filterDifference :: Steps -> Steps
        filterDifference [] = []
        filterDifference [x] = [x]
        filterDifference (x:y:xs) | getDifference x y = [x, y]
                                  | otherwise = [x]
                          where getDifference x y = abs (x - y) <= 2

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

-- =========================================================
-- Build Tab Diagram
-- =========================================================
buildTab :: [(SingleNote, Steps)] -> [String]
buildTab nt = do
    let bds = buildTabStrings nt
    let maxLen = maximum $ map length bds
    b <- bds
    return (b ++ genSpaces maxLen b ++ "-|")
    where
        buildTabStrings :: [(SingleNote, Steps)] -> [String]
        buildTabStrings xs = [formatNote (fst x) ++ "||" ++ showPositions (snd x) | x <- xs]

        showPositions :: Steps -> String
        showPositions [] = "--x"
        showPositions [x] = if x < 10 then "--" ++ show x
                          else "-" ++ show x
        showPositions (x:xs) = showPositions [x] ++ showPositions xs

        genSpaces :: Int -> String -> String
        genSpaces maxLen n
            | (maxLen - length n) == 0 = ""
            | otherwise = concat $ replicate (maxLen - length n) "-"

-- =========================================================
-- Build Fret Diagram with Symbols
-- =========================================================
buildFretSymbols :: [(SingleNote, Steps)] -> Int -> [String]
buildFretSymbols nt maxFret =
    [formatNote (fst ns) ++ fretSymbols maxFret (snd ns) | ns <- nt]
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
buildFretNotes root nt maxFret =
    [formatNote (fst ns) ++ fretNotes (fst ns) root maxFret (snd ns) | ns <- nt]
    where
        fretNotes :: SingleNote -> SingleNote -> Int -> Steps -> String
        fretNotes str root maxFret [] = "|x|" ++ concat (replicate maxFret "-----|")
        fretNotes str root maxFret (n:ns)
            | n == 0 = "|o|" ++ makeStringNotes str root maxFret ns
            | otherwise = "| |" ++ makeStringNotes str root maxFret (n:ns)

        makeStringNotes :: SingleNote -> SingleNote -> Int -> Steps -> String
        makeStringNotes str root maxFret ns = do
            fret <- [1..maxFret]
            if fret `elem` ns then
                if root `elem` keysWithSharps then "--" ++ flatSharp (nSteps str fret) ++ "-|"
                else "--" ++ formatNote (nSteps str fret) ++ "-|"
            else "-----|"

-- =========================================================
-- Helper functions to display the header and footer of the diagrams.
-- =========================================================
fretHeader :: SingleNote -> Int -> String
fretHeader root fret = "\nRoot: " ++ show root ++ "\nMin. Fret: " ++ show fret

fretFooter :: Int -> String
fretFooter maxFret = do
    let prt = partition (10>) [1..maxFret]
    "   0   " ++ intercalate "     " (map show (fst prt))
     ++ "     " ++ intercalate "    " (map show (snd prt)) ++ "\n"

-- =========================================================
-- Functions to output guitar things diagrams
-- =========================================================
-- Displays chromatic scale for each of the strings based on the tuning.
makeGuitar :: Notes -> IO()
makeGuitar tuning = do
    clearScreen
    let maxFret = 12
    putStrLn $ "\n" ++ unlines (map putNotes (allNotes maxFret))
    where
        allNotes maxFret = [chromaticScale maxFret n | n <- tuning]

-- Displays notes for one position for a chord/scale based on the given parameters.
makeOne :: Notes -> (SingleNote -> Notes) -> SingleNote -> IO()
makeOne tuning scale root = do
    clearScreen
    fret <- getFret
    let maxFret = fret + 4
    let ns = fingering2 tuning (scale root) fret maxFret
    optionsMenu (zip tuning ns) root fret maxFret
    where
        getFret :: IO Int
        getFret = do
            putStr "\nEnter minimum fret: "
            maybeFret <- getLine
            case readMaybe maybeFret of
                Just fret | fret >= 0 -> return fret
                _ -> getFret

        optionsMenu :: [(SingleNote, Steps)] -> SingleNote -> Int -> Int -> IO()
        optionsMenu nt root fret maxFret = do
            putStr "\n1. Tab\n2. Symbols\n3. Notes\n4. Re-enter minimum fret\nChoose an option: "
            choice <- getLine
            case choice of
                "1" ->
                    putStrLn (fretHeader root fret) >> mapM_ putStrLn (buildTab nt)
                "2" ->
                    output(fretHeader root fret
                           : buildFretSymbols nt maxFret
                           ++ [fretFooter maxFret])
                "3" ->
                    output(fretHeader root fret
                           : buildFretNotes root nt maxFret
                           ++ [fretFooter maxFret])

                "4" -> makeOne tuning scale root
                _ -> optionsMenu nt root fret maxFret

-- Displays all notes for a chord/scale, from the fret passed in to the 16th fret.
makeAll :: Notes -> (SingleNote -> Notes) -> SingleNote -> IO()
makeAll tuning scale root = do
    clearScreen
    let maxFret = 16
    let fret = 0
    let ns = fingering tuning (scale root) fret maxFret
    putStr "\n1. Symbols\n2. Notes\nChoose an option: "
    choice <- getLine
    case choice of
        "1" ->
            output(fretHeader root fret
                   : buildFretSymbols (zip tuning ns) maxFret
                   ++ [fretFooter maxFret])
        "2" ->
            output(fretHeader root fret
                   : buildFretNotes root (zip tuning ns) maxFret
                   ++ [fretFooter maxFret])
        _ -> makeAll tuning scale root

output :: [String] -> IO()
output = mapM_ putStrLn

clearScreen :: IO()
clearScreen = putStr "\ESC[2J"

-- NOTE:
-- Breaking out functionality for some of this stuff??
-- I don't understand the making these functions return String and not IO()??
