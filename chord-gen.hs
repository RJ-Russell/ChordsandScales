
data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum)

type Notes = [SingleNote]
type Steps = [Int]

data DiatonicMode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    deriving (Show)

genMode :: DiatonicMode -> Steps
genMode Ionian     = [0, 2, 4, 5, 7, 9, 11, 12]
genMode Dorian     = drop 1 (genMode Ionian)     ++ [2]
genMode Phrygian   = drop 1 (genMode Dorian)     ++ [4]
genMode Lydian     = drop 1 (genMode Phrygian)   ++ [5]
genMode Mixolydian = drop 1 (genMode Lydian)     ++ [7]
genMode Aeolian    = drop 1 (genMode Mixolydian) ++ [9]
genMode Locrian    = drop 1 (genMode Aeolian)    ++ [11]

data Triad = Major | Minor
    deriving (Show)

genTriad :: Triad -> Steps
genTriad Major = [0, 4, 7]
genTriad Minor = [0, 3, 7]


-- Increment a half step.
-- Calling `succ` on the last item of the data type produces an error,
-- therefore, pattern match the last note, and set the next thing to the
-- first note so that it wraps around
-- For all other notes, call `succ` on the note to get the sucessor.
halfStep :: SingleNote -> SingleNote
halfStep Ab = A
halfStep sn = succ sn

-- Returns the note that is 'n'-(half steps) away from the note
-- passed in. Generates a list of all the notes from the note passed in
-- and takes the one that is `x` steps away.
nSteps :: SingleNote -> Int -> SingleNote
nSteps sn x = iterate halfStep sn !! x

-- Generates a list of SingleNotes that are in the key (where the key is the root note)
-- which corresponds to the Ionian scale.
-- Later this will be changed to generate all scales in the key.
genDiatonicScale :: SingleNote -> DiatonicMode -> Notes
genDiatonicScale root scale = [nSteps root x | x <- genMode scale]

genTriadScale :: SingleNote -> Triad -> Notes
genTriadScale root scale = [nSteps root x | x <- genTriad scale]

-- Need a function that maps notes to strings.
-- Needs to take a Scale, and the tuning, and give back the positions
-- on each string where that note exists. Try to create a list of lists, where
-- each list element represents a string. Each inner list element should
-- be a list of Ints representing the fret position to play that note.

numFrets = 25
stdTuning = [E, A, D, G, B, E]

chromatic :: SingleNote -> Notes
chromatic n = take numFrets (iterate halfStep n)

genStrings :: [Notes]
genStrings = [chromatic n | n <- stdTuning]

makeString :: Notes -> String
makeString [x] = show x
makeString (x:xs) = show x ++ " " ++ makeString xs

makeGuitar :: IO()
makeGuitar = putStrLn . unlines $ map makeString genStrings
