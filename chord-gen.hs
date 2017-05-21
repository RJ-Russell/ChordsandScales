import Data.List
import Control.Monad

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


-- Generates a chromatic scale using the SingleNote passed in as the starting point.
chromaticScale :: SingleNote -> Notes
chromaticScale root = take numFrets (iterate halfStep root)

-- ===============================================

-- Generates list of indices where the notes in the scale are located on the
-- fretboard for a given string tuned to a specific note.
positions :: Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions tuning scale root = [getPos (scale root) | t <- tuning,
                                let fretPosition f s = [x | (n, x)
                                       <- zip (chromaticScale f) [0..], s == n],
                                let getPos ss = concat [fretPosition t s | s <- ss]]

filterDifference :: Steps -> Steps
filterDifference [] = []
filterDifference xs | getDifference xs <= 2 = xs
                    | otherwise = [minimum xs]
                    where getDifference xs = abs $ foldr (-) 0 xs

-- Generates a list of positions on the fretboard for a chord at a given fret.
-- chords :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Maybe Int]
-- chords tuning scale root pos = map (take 2 . sort . filter (pos<=))
--                                $ positions tuning scale root
-- filterFrets fret [] = []
-- filterFrets fret (x:xs)  | (x - fret) <= 3 = x : filterFrets fret xs
--                          | otherwise = filterFrets fret (delete x xs)

chordFingering :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Steps]
chordFingering tuning scale root pos = map (filterDifference . take 2 . sort . filter (pos<=))
                                       $ positions tuning scale root

buildDisplay :: [(SingleNote, Steps)] -> [String]
buildDisplay xs = [makeString x | x <- xs]
                        where makeString (n,[])  = show n ++ " |--" ++ showPositions []
                              makeString (n,[x]) = show n ++ " |--" ++ showPositions [x]
                              makeString (n,xs)  = show n ++ " |--" ++ showPositions xs
                              showPositions [] = "  x "
                              showPositions [x] | x < 10 = "  " ++ show x ++ " "
                                                        | otherwise = " " ++ show x ++ " "
                              showPositions (x:xs) = showPositions [x] ++ showPositions xs

-- ===============================================

putNotes :: Notes -> String
putNotes [n] = show n
putNotes (n:ns) = show n ++ " " ++ putNotes ns

putSteps :: Steps -> String
putSteps [n] = show n
putSteps (n:ns) = show n ++ " " ++ putSteps ns


-- Functions to output guitar things.
makeGuitar :: Notes -> IO()
makeGuitar tuning = putStrLn $ unlines $ map putNotes (allNotes tuning)
            where allNotes tuning = [chromaticScale n | n <- tuning]

-- makeChord :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> IO()
makeChord tuning scale root fret = fretHeader >> mapM_ putStrLn getStrings
    where fretHeader = putStrLn ("\nMin. Fret: " ++ show fret ++ "\nChord: " ++ show root)
          getStrings = do
                        let ns     = chordFingering tuning scale root fret
                        let nt     = zip tuning ns
                        let bds    = buildDisplay nt
                        let maxLen = maximum $ map length bds
                        b <- bds
                        return (b ++ genSpaces maxLen b ++ "--|")

genSpaces :: Int -> String -> String
genSpaces maxLen n | (maxLen - length n) == 0 = ""
                   | otherwise = concat $ replicate (maxLen - length n) " "

-- Zip lists like this: [E, A] [[1,2,3], [3,4,5]] = [(E, 1), (E, 2), (E, 3), (A, 3), (A, 4), (A, 5)]
--      so zip [E, A] [[1,2,3], [4,5,6]] = (E, [1,2,3]), (A, [4,5,6])
-- Error check initial input? hmmm... I dont need to, but it might be nice?
-- User interface to select things?
