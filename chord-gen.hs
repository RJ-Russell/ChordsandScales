import Data.List

data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq)

-- Give more meaningful names.
type Notes = [SingleNote]
type Steps = [Int]

-- There are 0..24 Frets on this guitar.
numFrets :: Int
numFrets = 25

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
chromaticScale :: SingleNote -> Notes
chromaticScale root = take numFrets (iterate halfStep root)

-- ===============================================

-- Generates list of indices where the notes in the scale are located on the
-- fretboard for a given string tuned to a specific note.
positions :: Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions tuning scale root = [getPos (scale root) | t <- tuning,
                                let fretPosition s = elemIndices s (chromaticScale t),
                                let getPos ss = concat [fretPosition s | s <- ss]]

filterDifference :: Steps -> Steps
filterDifference [] = []
filterDifference xs | getDifference xs <= 2 = xs
                    | otherwise = [minimum xs]
                    where getDifference xs = abs $ foldr (-) 0 xs

-- Generates a list of Steps for each note in the scale per string.
scaleFingering :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Steps]
scaleFingering tuning scale root pos = map (nub . sort . filter (pos<=))
                                         $ positions tuning scale root

-- Generates a list of Steps for each note in the chord per string.
chordFingering :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Steps]
chordFingering tuning scale root pos = map (filterDifference . take 2 . sort . filter (pos<=))
                                       $ positions tuning scale root

-- Builds a single ASCII string.
buildDisplay :: [(SingleNote, Steps)] -> [String]
buildDisplay xs = [makeString x | x <- xs]
                        where makeString (s,[])  = showPositions []
                              makeString (s,[x]) = showPositions [x]
                              makeString (s,xs)  = showPositions xs
                              showPositions [] = " x"
                              showPositions [x] | x < 10 = " " ++ show x
                                                | otherwise = show x
                              showPositions (x:xs) = showPositions [x] ++ " (" ++ showPositions xs ++ ")"

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

-- NOTE: ?? What to do with this ??
getStrings :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int
              -> (Notes -> (SingleNote -> Notes) -> SingleNote
              -> Int -> [Steps]) -> [String]
getStrings tuning scale root fret fingering = do
                                     let ns     = fingering tuning scale root fret
                                     let nt     = zip tuning ns
                                     let bds    = buildDisplay nt
                                     let maxLen = maximum $ map length bds
                                     b <- bds
                                     return ("||-- " ++ b ++ genSpaces maxLen b ++ " --|")

fretHeader :: SingleNote -> Int -> IO()
fretHeader root fret = putStrLn ("\nMin. Fret: " ++ show fret ++ "\nChord: " ++ show root)

genSpaces :: Int -> String -> String
genSpaces maxLen n | (maxLen - length n) == 0 = ""
                   | otherwise = concat $ replicate (maxLen - length n) " "

-- Functions to output guitar things.
makeGuitar :: Notes -> IO()
makeGuitar tuning = putStrLn $ unlines $ map putNotes (allNotes tuning)
            where allNotes tuning = [chromaticScale n | n <- tuning]

makeChord, makeScale :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> IO()
makeChord tuning scale root fret = fretHeader root fret >> mapM_ putStrLn (getStrings tuning scale root fret chordFingering)
makeScale tuning scale root fret = fretHeader root fret >> mapM_ putStrLn (getStrings tuning scale root fret scaleFingering)
