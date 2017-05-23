import Data.List
import Control.Monad (when)

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
halfStep n  = succ n

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
chromaticScale maxFrets root = take maxFrets (iterate halfStep root)

-- ===============================================

-- Generates list of indices where the notes in the scale are located on the
-- fretboard for a given string tuned to a specific note.
positions :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions maxFrets tuning scale root = [getPos (scale root) | t <- tuning,
                                let fretPosition s = elemIndices s (chromaticScale maxFrets t),
                                let getPos ss = concat [fretPosition s | s <- ss]]

-- filterDifference :: Steps -> Steps
-- filterDifference [] = []
-- filterDifference xs | getDifference xs <= 2 = xs
--                     | otherwise = [minimum xs]
--                     where getDifference xs = abs $ foldr (-) 0 xs

-- Generates a list of Steps for each note in the scale per string.
scaleFingering :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Steps]
scaleFingering maxFrets tuning scale root pos = map (nub . sort . filter (pos<=))
                                         $ positions maxFrets tuning scale root

-- Generates a list of Steps for each note in the chord per string.
chordFingering :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Steps]
chordFingering maxFrets tuning scale root pos = map (take 1 . sort . filter (pos<=))
                                       $ positions maxFrets tuning scale root

-- ===============================================

-- Functions for Output
-- ===============================================
putNotes :: Notes -> String
putNotes [n] = show n
putNotes (n:ns) = show n ++ " " ++ putNotes ns

-- putSteps :: Steps -> String
-- putSteps [] = "x"
-- putSteps [n] = show n
-- putSteps (n:ns) = show n ++ " " ++ putSteps ns

-- Builds an array of single ASCII string.
buildTabStrings :: [(SingleNote, Steps)] -> [String]
buildTabStrings xs = [formatTuning (fst x) ++ "||" ++ showPositions (snd x) | x <- xs]
                        where showPositions []              = "--x"
                              showPositions [x] | x < 10    = "- " ++ show x
                                                | otherwise = "-" ++ show x
                              showPositions (x:xs)          = showPositions [x] ++ showPositions xs
                              formatTuning n | 'b' `elem` show n = show n ++ " "
                                             | otherwise           = show n ++ "  "

buildTab :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [String]
buildTab maxFrets tuning scale root fret = do
                                     let ns     = chordFingering maxFrets tuning scale root fret
                                     let nt     = zip tuning ns
                                     let bds    = buildTabStrings nt
                                     let maxLen = maximum $ map length bds
                                     b <- bds
                                     return (b ++ genSpaces maxLen b ++ "-|")
                   where genSpaces maxLen n | (maxLen - length n) == 0 = ""
                                            | otherwise = concat $ replicate (maxLen - length n) "-"


buildFretStrings :: Int -> [(SingleNote, Steps)] -> [String]
buildFretStrings maxFret nt = [formatTuning (fst ns) ++ (concat $ helpmeee maxFret (snd ns)) | ns <- nt]
                        where makeNeck fret n ns | fret == n = "|o|"
                                                 | otherwise = "| |"
                              makeStrings fret n ns | fret == n = "--o--|"
                                                    | otherwise = "-----|"
                              helpmeee maxFrets ns = if null ns then
                                                      return ("|x|" ++ concat (replicate maxFrets "-----|"))
                                                     else do
                                                            fret <- [0..maxFrets]
                                                            n <- ns
                                                            if fret == 0 then return (makeNeck fret n ns)
                                                            else return (makeStrings fret n ns)
                              formatTuning n | 'b' `elem` show n = show n ++ " "
                                             | otherwise = show n ++ "  "

buildFrets :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [String]
buildFrets maxFrets tuning scale root fret  = do
                                               let ns = chordFingering maxFrets tuning scale root fret
                                               let nt = zip tuning ns
                                               let bfs = buildFretStrings maxFrets nt
                                               bfs

fretHeader :: SingleNote -> Int -> IO()
fretHeader root fret = putStrLn ("\nChord: " ++ show root ++ "\nMin. Fret: " ++ show fret)

-- Functions to output guitar things.
makeGuitar :: Int -> Notes -> IO()
makeGuitar maxFrets tuning = putStrLn $ unlines $ map putNotes (allNotes tuning)
            where allNotes tuning = [chromaticScale maxFrets n | n <- tuning]

makeChordTab :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> IO()
makeChordTab maxFrets tuning scale root fret = fretHeader root fret
                                                >> mapM_ putStrLn (buildTab maxFrets tuning scale root fret)

makeChordFrets :: Int -> Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> IO()
makeChordFrets maxFrets tuning scale root fret = fretHeader root fret >> mapM_ putStrLn (buildFrets maxFrets tuning scale root fret)
