{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module Main where

import           Criterion.Main
import           GHC.Prim
import           GHC.Types

import qualified GHC.Integer         as Y
import qualified OpenSSL.GHC.Integer as X

main :: IO ()
main = defaultMain
  [ bgroup "mkInteger"
    [ bgroup "128bit"
      [ bench "library" $ whnf (mkIntegerBench X.mkInteger) big128
      , bench "builtin" $ whnf (mkIntegerBench Y.mkInteger) big128
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf (mkIntegerBench Y.mkInteger) big4096
      , bench "builtin" $ whnf (mkIntegerBench Y.mkInteger) big4096
      ]
    ]
  , bgroup "timesInteger"
    [ bgroup "small"
      [ bench "library" $ whnf timesIntegerX (Small 123, Small 42)
      , bench "builtin" $ whnf timesIntegerY (Small 123, Small 42)
      ]
    , bgroup "128bit"
      [ bench "library" $ whnf timesIntegerX (big128, big128_)
      , bench "builtin" $ whnf timesIntegerY (big128, big128_)
      ]
    , bgroup "4096bit"
      [ bench "library" $ whnf timesIntegerX (big4096, big4096_)
      , bench "builtin" $ whnf timesIntegerY (big4096, big4096_)
      ]
    ]
  ]
 where
  big128 = Big False [0x1, 0x1, 0x1, 0x1, 0xf] -- 4*31+4 = 128
  big128_ = Big True [0x2, 0x1, 0x1, 0x1, 0xf]
  big4096 = Big True $ [0x0..0x84] ++ [0xf] -- 132*31+4 = 4096
  big4096_ = Big False $ [0x1..0x85] ++ [0xf]
  timesIntegerX = timesIntegerBench X.mkInteger X.smallInteger X.timesInteger
  timesIntegerY = timesIntegerBench Y.mkInteger Y.smallInteger Y.timesInteger

data IntegerBench = Small Int
                  | Big Bool [Int]

-- | Benchmark big integer creation.
mkIntegerBench :: (Bool -> [Int] -> a) -> IntegerBench -> a
mkIntegerBench mkInteger (Big p is) = mkInteger p is

timesIntegerBench :: (Bool -> [Int] -> a) -> (Int# -> a) -> (a -> a -> a)
                  -> (IntegerBench, IntegerBench) -> a
timesIntegerBench mkInteger smallInteger timesInteger = go
 where
  go (Small (I# i1#), Small (I# i2#)) = timesInteger (smallInteger i1#) (smallInteger i2#)
  go (Big p is, Small (I# i#)) = timesInteger (mkInteger p is) (smallInteger i#)
  go (Small (I# i#), Big p is) = timesInteger (smallInteger i#) (mkInteger p is)
  go (Big p is, Big p2 is2) = timesInteger (mkInteger p is) (mkInteger p2 is2)
