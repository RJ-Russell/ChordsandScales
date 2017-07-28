# Haskell Chord/Scale Generator
### Requirements
In order to run this program, install `ghc` and use
`ghci`.  
Please refer to the Haskell documentation pertaining to your platform for instructions on installing `ghc`.

### Usage
Start ghci  
`$ ghci`  
Load chord-gen.hs  
`Prelude> :l chord-gen.hs`  

There is one function to display the chromatic scale for each string based on the tuning:
  * ```buildGuitar <tuning>```

There are two possible functions to generate a chord or a scale:
  * ```buildSingle <tuning> <scale> <root note>```
  * ```buildAll <tuning> <scale> <root note>```

For the ```buildSingle``` function, there are three options that can be selected in the menu prompts:
  1. Tabs
  2. Symbols
  3. Notes

Example output of each:
```
*Main> buildSingle standard majTriad G
Enter minimum fret (0 - 12): 0
1. Tab
2. Symbols
3. Notes
Enter choice: 1

Root: G
Min. Fret: 0
E ||--3-|
B ||--0-|
G ||--0-|
D ||--0-|
A ||--2-|
E ||--3-|
*Main>
```

```
*Main> buildSingle standard majTriad G
Enter minimum fret (0 - 12): 0
1. Tab
2. Symbols
3. Notes
Enter choice: 2

Root: G
Min. Fret: 0
E | |-----|-----|--o--|-----|
B |o|-----|-----|-----|-----|
G |o|-----|-----|-----|-----|
D |o|-----|-----|-----|-----|
A | |-----|--o--|-----|-----|
E | |-----|-----|--o--|-----|
   0   1     2     3     4     
*Main> 
```

```
*Main> buildSingle standard majTriad G
Enter minimum fret (0 - 12): 0
1. Tab
2. Symbols
3. Notes
Enter choice: 3

Root: G
Min. Fret: 0
E | |-----|-----|--G--|-----|
B |o|-----|-----|-----|-----|
G |o|-----|-----|-----|-----|
D |o|-----|-----|-----|-----|
A | |-----|--B--|-----|-----|
E | |-----|-----|--G--|-----|
   0   1     2     3     4     
*Main> 
```

There are two options for the `buildAll` function:
  1. Symbols
  2. Notes
 
Example output of each:
```
*Main> buildAll standard majTriad G
1. Symbols
2. Notes
Enter choice: 1

Root: G
Min. Fret: 0
E | |-----|-----|--o--|-----|-----|-----|--o--|-----|-----|--o--|-----|-----|-----|-----|--o--|-----|
B |o|-----|-----|--o--|-----|-----|-----|-----|--o--|-----|-----|-----|--o--|-----|-----|--o--|-----|
G |o|-----|-----|-----|--o--|-----|-----|--o--|-----|-----|-----|-----|--o--|-----|-----|-----|--o--|
D |o|-----|-----|-----|-----|--o--|-----|-----|-----|--o--|-----|-----|--o--|-----|-----|-----|-----|
A | |-----|--o--|-----|-----|--o--|-----|-----|-----|-----|--o--|-----|-----|-----|--o--|-----|-----|
E | |-----|-----|--o--|-----|-----|-----|--o--|-----|-----|--o--|-----|-----|-----|-----|--o--|-----|
   0   1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16
*Main> 
```

```
*Main> buildAll standard majTriad G
1. Symbols
2. Notes
Enter choice: 2

Root: G
Min. Fret: 0
E | |-----|-----|--G--|-----|-----|-----|--B--|-----|-----|--D--|-----|-----|-----|-----|--G--|-----|
B |o|-----|-----|--D--|-----|-----|-----|-----|--G--|-----|-----|-----|--B--|-----|-----|--D--|-----|
G |o|-----|-----|-----|--B--|-----|-----|--D--|-----|-----|-----|-----|--G--|-----|-----|-----|--B--|
D |o|-----|-----|-----|-----|--G--|-----|-----|-----|--B--|-----|-----|--D--|-----|-----|-----|-----|
A | |-----|--B--|-----|-----|--D--|-----|-----|-----|-----|--G--|-----|-----|-----|--B--|-----|-----|
E | |-----|-----|--G--|-----|-----|-----|--B--|-----|-----|--D--|-----|-----|-----|-----|--G--|-----|
   0   1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16
*Main> 
```

### Current \<tuning\> options
```
standard: E, A, D, G, B, E
dropD: D, A, D, G, B, E
halfDown: Eb, Ab, Db, Gb, Bb, Eb
fullDown: D, G, C, F, A, D
openE: E, B, E, Ab, B, E
sevenRock: B, E, A, D, G, B, E
ukulele: A, E, C, G
```

### Current \<scale\> list
```
majTriad
minTriad
maj7th
min7th
sus4
sus2
diatonicMaj
diatonicMin
pentatonicMaj
pentatonicMin
```

## Root Notes
Root notes are from A, Bb, B, C, ..., Ab. For sharp notes, only the corresponding flat reprentation is supported. Although, the output will include sharps where appropriate.
