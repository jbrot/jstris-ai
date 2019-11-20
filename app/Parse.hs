module Parse where

import Text.Printf

data Square = Empty | Garbage 
                  | I | IShadow
                  | J | JShadow 
                  | L | LShadow 
                  | O | OShadow 
                  | S | SShadow
                  | T | TShadow
                  | Z | ZShadow
    deriving Eq

-- This should probably be turned into a better structure than a list of tuples.
colorMap = [ ((  0,  0,  0), Empty)
           , ((153,153,153), Garbage)
           , (( 15,155,215), I)
           , ((  7, 77,107), IShadow)
           , (( 33, 65,198), J)
           , (( 16, 32, 99), JShadow)
           , ((227, 91,  2), L)
           , ((113, 45,  1), LShadow)
           , ((227,159,  2), O)
           , ((113, 79,  1), OShadow)
           , (( 89,177,  1), S)
           , (( 44, 88,  0), SShadow)
           , ((175, 41,138), T)
           , (( 87, 20, 69), TShadow)
           , ((215, 15, 55), Z)
           , ((107,  7, 27), ZShadow)
           ]
charMap = [ (' ', Empty)
          , ('X', Garbage)
          , ('I', I)
          , ('I', IShadow)
          , ('J', J)
          , ('J', JShadow)
          , ('L', L)
          , ('L', LShadow)
          , ('O', O)
          , ('O', OShadow)
          , ('S', S)
          , ('S', SShadow)
          , ('T', T)
          , ('T', TShadow)
          , ('Z', Z)
          , ('Z', ZShadow)
          ]

parseSquare :: Int -> Int -> Int -> Square
parseSquare r g b = snd . head . filter ((== (r,g,b)) . fst) $ colorMap

renderSquare :: Square -> (Int, Int, Int)
renderSquare c = fst . head . filter ((== c) . snd) $ colorMap

renderChar :: Square -> Char
renderChar c = fst . head . filter ((== c) . snd) $ charMap

-- Parses the raw pixel data into a 20 x 10 grid of Squares.
parseFrame :: [Int] -> [[Square]]
parseFrame cs = go 0 0 cs
    where go :: Int -> Int -> [Int] -> [[Square]]
          go  20  _ _ = []
          go row 10 colors = []:(go (row + 1) 0 colors)
          go row col (r:g:b:a:colors) = let (rr:rs) =  go row (col + 1) colors in (parseSquare r g b : rr):rs
          go _ _ _ = [[]]

-- Print the given frame to the terminal, assuming 24 bit color is supported.
-- Without 24 bit color, this will print nonsense.
printFrame :: [[Square]] -> IO ()
printFrame = (>> return ()) . sequence . fmap printRow
  where printChar :: Square -> IO ()
        printChar c = let (r,g,b) = renderSquare c in printf "\x1b[48;2;%d;%d;%dm%c" r g b (renderChar c)
        printRow :: [Square] -> IO ()
        printRow = (>> printf "\x1b[0m\n") . sequence . fmap printChar
