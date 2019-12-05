import Criterion.Main
import Data.Char
import Text.Printf

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

main = let
  startWhiteSpaces :: String
  startWhiteSpaces = "   "

  endWhiteSpaces :: String
  endWhiteSpaces = " \t   "

  short    = 5
  middle   = 50
  long     = 100
  veryLong = 1000

  shortStr :: String
  shortStr = startWhiteSpaces ++ replicate short 'a' ++ endWhiteSpaces

  middleStr :: String
  middleStr = startWhiteSpaces ++ replicate middle 'a' ++ endWhiteSpaces

  longStr :: String
  longStr = startWhiteSpaces ++ replicate long 'a' ++ endWhiteSpaces

  veryLongStr :: String
  veryLongStr = startWhiteSpaces ++ replicate veryLong 'a' ++ endWhiteSpaces

  in defaultMain [
       bgroup "strip" [ bench "foo bar"   $ whnf strip " foo bar     "
                      , bench (printf "short (len:%d)" short)         $ whnf strip shortStr
                      , bench (printf "middle (len:%d)" middle)       $ whnf strip middleStr
                      , bench (printf "long (len: %d)" long)          $ whnf strip longStr
                      , bench (printf "very long (len: %d)" veryLong) $ whnf strip veryLongStr
                      ]
       ]
