import Data.List

data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq)

-- Give more meaningful names.
type Notes = [SingleNote]
type Steps = [Int]

-- Give the guitar 1 octave (0-12 = 13).
--  NOTE: This will change later once I figure out how to represent notes an
--  octave higher.
numFrets :: Int
numFrets = 13

-- Gets the successor of the SingleNote passed in. Wraps around so that
-- the datatype SingleNote is circular.
halfStep :: SingleNote -> SingleNote
halfStep Ab = A
halfStep n  = succ n

-- Returns a SingleNote that is `n` steps from the SingleNote passed in.
nSteps :: SingleNote -> Int -> SingleNote
nSteps n x = iterate halfStep n !! x

-- Generates a scale of SingleNotes, using the SingleNote argument
-- as the starting point and the a predefined array of int steps as the scale.
genScale :: SingleNote -> Steps -> Notes
genScale n sc = [nSteps n x | x <- sc]

-- Scales that are defined.
ionian :: SingleNote -> Notes
ionian root = genScale root [0, 2, 4, 5, 7, 9, 11, 12]

majTriad :: SingleNote -> Notes
majTriad root = genScale root [0, 4, 7]

-- Generates a chromatic scale using the SingleNote passed in as the starting point.
chromaticScale :: SingleNote -> Notes
chromaticScale n = take numFrets (iterate halfStep n)

-- Generates a chromatic scale of SingleNotes for each note in the tuning passed in.
allNotes :: Notes -> [Notes]
allNotes tuning = [chromaticScale n | n <- tuning]

fretPosition :: SingleNote -> SingleNote -> Int
fretPosition f s = length $ takeWhile (/=s) (iterate halfStep f)

getPos :: [SingleNote] -> SingleNote -> Steps
getPos scale n = [fretPosition n s | s <- scale]

positions :: Notes -> (SingleNote -> Notes) -> SingleNote -> [Steps]
positions tuning scale root = [getPos (scale root) t | t <- tuning]


chordsPosZero tuning scale root = map head $ map sort $ positions tuning scale root




-- ====== TUNINGS ===============================
standard :: Notes
standard = [E, A, D, G, B, E]

dropD :: Notes
dropD = [D, A, D, G, B, E]

halfDown :: Notes
halfDown = [Eb, Ab, Db, Gb, Bb, Eb]

fullDown :: Notes
fullDown = [D, G, C, F, A, D]

-- ==============================================


putString :: Notes -> String
putString [n] = show n
putString (n:ns) = show n ++ " " ++ putString ns

putAllStrings :: Notes -> IO()
putAllStrings tuning = putStrLn $ unlines $ map putString (allNotes tuning)


main :: IO()
main = print "hello"
