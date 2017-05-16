
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

ionian :: SingleNote -> Notes
ionian root = genScale root [0, 2, 4, 5, 7, 9, 11, 12]

majTriad :: SingleNote -> Notes
majTriad root = genScale root [0, 4, 7]

minTriad :: SingleNote -> Notes
minTriad root = genScale root [0, 3, 7]

-- ====== TUNINGS ================================
standard :: Notes
standard = [E, A, D, G, B, E]

dropD :: Notes
dropD = [D, A, D, G, B, E]

halfDown :: Notes
halfDown = [Eb, Ab, Db, Gb, Bb, Eb]

fullDown :: Notes
fullDown = [D, G, C, F, A, D]


-- Generates a chromatic scale using the SingleNote passed in as the starting point.
chromaticScale :: SingleNote -> Notes
chromaticScale root = take numFrets (iterate halfStep root)

-- ===============================================

-- Generates a chromatic scale of SingleNotes for each note in the tuning passed in.
allNotes :: Notes -> [Notes]
allNotes tuning = [chromaticScale n | n <- tuning]

-- Generates list of indices where the notes in the scale are located on the
-- fretboard for a given string tuned to a specific note.
positions :: Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions tuning scale root = [getPos (scale root) | t <- tuning,
                                let fretPosition f s = [x | (n, x)
                                       <- zip (chromaticScale f) [0..], s == n],
                                let getPos ss = concat [fretPosition t s | s <- ss]]

-- Generates a list of positions on the fretboard for a chord at a given fret.
chords :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> [Maybe Int]
chords tuning scale root pos = map (getMinimum . filter (pos<=))
                               $ positions tuning scale root

getMinimum :: Steps -> Maybe Int
getMinimum [] = Nothing
getMinimum xs = Just (minimum xs)

displayFretPosition :: Maybe Int -> String
displayFretPosition x | x == Nothing = "  x"
                      | x < Just 10  = "  " ++ show (toInt x)
                      | otherwise    = " " ++ show (toInt x)
                      where toInt (Just x) = x

-- NOTE: This is not being used. Remove later??
-- filterRange :: Int -> Steps -> Steps
-- filterRange _ [] = []
-- filterRange pos (x:xs) | inRange = x : filterRange pos xs
--                        | otherwise = filterRange pos xs
--                        where inRange = pos <= x && x <= pos + 4

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

makeChord :: Notes -> (SingleNote -> Notes) -> SingleNote -> Int -> IO()
makeChord tuning scale root fret = fretHeader >> mapM_ putStrLn getStrings
    where fretHeader = putStrLn ("\nMin. Fret: " ++ show fret ++ "\nChord: " ++ show root)
          getStrings = do
                        let n = reverse $ chords tuning scale root fret
                        let t = reverse tuning
                        z <- zip t n
                        return (show (fst z) ++ " ||--" ++ displayFretPosition (snd z) ++ " --|")

-- Zip lists like this: [E, A] [[1,2,3], [3,4,5]] = [(E, 1), (E, 2), (E, 3), (A, 3), (A, 4), (A, 5)]
--      so zip [E, A] [[1,2,3], [4,5,6]] = (E, [1,2,3]), (A, [4,5,6])
-- Error check initial input? hmmm... I dont need to, but it might be nice?
-- User interface to select things?
