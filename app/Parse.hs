{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Data.Text (Text)
import qualified Test.WebDriver.Common.Keys as K
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

isShadow :: Square -> Bool
isShadow IShadow = True 
isShadow JShadow = True
isShadow LShadow = True
isShadow OShadow = True
isShadow SShadow = True
isShadow TShadow = True
isShadow ZShadow = True
isShadow _ = False

-- Parses the raw pixel data into a 20 x 10 grid of Squares.
parseFrame :: [Int] -> [[Square]]
parseFrame cs = go 0 0 cs
    where go :: Int -> Int -> [Int] -> [[Square]]
          go  20  _ _ = []
          go row 10 colors = []:(go (row + 1) 0 colors)
          go row col (r:g:b:a:colors) = let (rr:rs) =  go row (col + 1) colors in (parseSquare r g b : rr):rs
          go _ _ _ = [[]]

tagArray :: [[a]] -> [[((Int, Int), a)]]
tagArray = fmap (\(r, bs) -> fmap (\(c,v) -> ((r,c),v)) bs) . zip [1..] . fmap (zip [1..])

-- Finds the coordinates that have a shadow in them.
shadowIndexes :: [[Square]] -> [(Int, Int)]
shadowIndexes = mconcat . fmap (\(a,bs) -> fmap ((,) a) bs) . filter (not . null . snd) . zip [1..] . fmap rowIndexes
    where rowIndexes :: [Square] -> [Int]
          rowIndexes = fmap fst . filter (isShadow . snd) . zip [1..]

-- Eliminates shadow and replaces the non-moving parts with garbage.
-- May not always work.
filterInactive :: [[Square]] -> [[Square]]
filterInactive cs = fmap (\(r,xs) -> fmap proc . zip ((,) r <$> [1..]) $ xs) . zip [1..] $ cs
    where sinds = let l = shadowIndexes cs in if null l then [(0,0)] else l
          minCol = minimum (snd <$> sinds)
          maxCol = maximum (snd <$> sinds)
          minRow = minimum (fst <$> sinds)
          proc :: ((Int, Int), Square) -> Square
          proc (_, Empty) = Empty
          proc (_, Garbage) = Garbage
          proc ((row, col), c)
            | isShadow c    = Empty
            | row >= minRow = Garbage 
            | col < minCol  = Garbage
            | col > maxCol  = Garbage
            | otherwise     = c

-- Print the given frame to the terminal, assuming 24 bit color is supported.
-- Without 24 bit color, this will print nonsense.
printFrame :: [[Square]] -> IO ()
printFrame = (>> return ()) . sequence . fmap printRow
  where printChar :: Square -> IO ()
        printChar c = let (r,g,b) = renderSquare c in printf "\x1b[48;2;%d;%d;%dm%c" r g b (renderChar c)
        printRow :: [Square] -> IO ()
        printRow = (>> printf "\x1b[0m\n") . sequence . fmap printChar

data Action = MoveLeft | MoveRight | SoftDrop | HardDrop | RotateLeft | RotateRight | Hold

actionToText :: Action -> Text
actionToText MoveLeft    = K.arrowLeft
actionToText MoveRight   = K.arrowRight
actionToText SoftDrop    = K.arrowDown
actionToText HardDrop    = " "
actionToText RotateLeft  = "z"
actionToText RotateRight = K.arrowUp
actionToText Hold        = "c"
