
data SingleNote = A | Bb | B | C | Db | D | Eb | E | F | Gb | G | Ab
    deriving (Show, Enum, Eq, Ord)

singleStep :: SingleNote -> SingleNote
singleStep Ab = A
singleStep sn = succ sn

nSteps :: SingleNote -> Int -> SingleNote
nSteps sn x = iterate singleStep sn !! x

ionianScale = [0, 2, 4, 5, 7, 9, 11]

genScale :: SingleNote -> [SingleNote]
genScale root = [nSteps root x | x <- ionianScale]

numFrets = [0..12]
stdTune = [E, A, D, G, B, E]

