{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module Main where

import           Criterion.Main
import           GHC.Prim
import           Data.Bits           ((.&.))
import           Test.QuickCheck     hiding ((.&.))

import qualified GHC.Integer         as Y
import qualified OpenSSL.GHC.Integer as X

main :: IO ()
main = defaultMain
  [ bgroup "mkInteger"
    [ bench "Library" $ whnf mkIntegerBench X.mkInteger
    , bench "Builtin" $ whnf mkIntegerBench Y.mkInteger
    ]
  , bgroup "mkInteger"
    [ bench "Library" $ whnf (timesSmallBench X.smallInteger) X.timesInteger
    , bench "Builtin" $ whnf (timesSmallBench Y.smallInteger) Y.timesInteger
    ]
  ]

-- | Benchmark integer creation from 255 31bit Ints.
mkIntegerBench :: (Bool -> [Int] -> a) -> a
mkIntegerBench mkInteger = mkInteger True [0x00 .. 0xff]

-- From: haskell-big-integer-experiment
{-# NOINLINE timesSmallBench #-}
timesSmallBench :: (Int# -> a) -> (a -> a -> a) -> a
timesSmallBench smallInteger timesInteger =
  loop 20000 count value
 where
  -- loop :: Int -> Int -> Integer -> Integer
  loop !0 !0 !accum = accum
  loop !k !0 !_     = loop (k - 1) count value
  loop !k !j !accum = loop k (j - 1) (timesInteger accum value)

  value = smallInteger 3#
  count = 32      -- 3 ^ 32 < 0x7fffffffffffffff
