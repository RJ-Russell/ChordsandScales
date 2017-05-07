
data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq, Ord)

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

-- Define the Ionian scale. Each number represents how many SingleNotes
-- there are to the next note in the scale.
ionianScale = [0, 2, 4, 5, 7, 9, 11]
aoliyanScale = [2, 4, 5, 7, 9, 11, 0]

-- Generates a list of SingleNotes that are in the key (where the key is the root note)
-- which corresponds to the Ionian scale.
-- Later this will be changed to generate all scales in the key.
genScale :: SingleNote -> [SingleNote]
genScale root = [nSteps root x | x <- aoliyanScale]

stdTune = [E, A, D, G, B, E]

