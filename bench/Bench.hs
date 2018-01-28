module Main where

import           Criterion.Main
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
  ]

-- | Benchmark integer creation from 255 31bit Ints.
mkIntegerBench :: (Bool -> [Int] -> a) -> a
mkIntegerBench mkInteger = mkInteger True [0x00 .. 0xff]
