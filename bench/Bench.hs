module Main where

import           Criterion.Main
import           Data.Bits           ((.&.))
import           Test.QuickCheck     hiding ((.&.))

import qualified GHC.Integer         as Y
import qualified OpenSSL.GHC.Integer as X

main :: IO ()
main = defaultMain
  [ env generateIntegerParams $ \params ->
    bgroup "mkInteger"
    [ bench "Library" $ whnf (mkIntegerBench X.mkInteger) params
    , bench "Builtin" $ whnf (mkIntegerBench Y.mkInteger) params
    ]
  ]

mkIntegerBench :: (Bool -> [Int] -> a) -> [(Bool, [Int])] -> a
mkIntegerBench mkInteger [] = mkInteger True [1]
mkIntegerBench mkInteger ((b, ints):ps) =
  (mkInteger b ints) `seq` (mkIntegerBench mkInteger ps)

generateIntegerParams :: IO [(Bool, [Int])]
generateIntegerParams =
  generate . vectorOf 1000 $ oneof [small, big]
 where
  small = do
    positive <- arbitrary
    i <- arbitrary
    pure (positive, i)

  big = do
    positive <- arbitrary
    ints <- map truncate32pos <$> arbitrary -- 31bit int chunks
    pure (positive, ints)

-- Truncate to a positive 32bit integer (required for mkInteger)
truncate32pos :: Int -> Int
truncate32pos i = abs i .&. 0x7fffffff
