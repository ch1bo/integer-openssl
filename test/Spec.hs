{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric (showHex)
import Data.Bits ((.&.))
import Data.Char (intToDigit)
import GHC.Prim
import GHC.Types
import Test.Hspec (hspec, describe, it, shouldBe, Expectation)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

import qualified OpenSSL.GHC.Integer as X -- this library
import qualified GHC.Integer as Y -- builtin, usually integer-gmp

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
# define INT_MINBOUND      -0x8000000000000000
# define INT_MAXBOUND       0x7fffffffffffffff
# define ABS_INT_MINBOUND   0x8000000000000000
# define WORD_SIZE_IN_BYTES 8
# define WORD_SHIFT         3
#elif WORD_SIZE_IN_BITS == 32
# define INT_MINBOUND       -0x80000000
# define INT_MAXBOUND       0x7fffffff
# define ABS_INT_MINBOUND   0x80000000
# define WORD_SIZE_IN_BYTES 4
# define WORD_SHIFT         2
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

main :: IO ()
main = do
  hspec $ do
    describe "Integer vs builtin" $ do
      describe "smallIntger" $ do
        prop "works for random Int#" $ \(SmallInt (I# i)) ->
          showHexX (X.smallInteger i) === showHexY (Y.smallInteger i)

      describe "mkInteger" $ do
        it "can create some Integers" $ do
          shouldEqualHex (X.mkInteger True [0xbb, 0xaa])
                         (Y.mkInteger True [0xbb, 0xaa])
          shouldEqualHex (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
                         (Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
        prop "can create random Integers" $ \(b, is) ->
          let ints = map truncate32pos is
          in  showHexX (X.mkInteger b ints) === showHexY (Y.mkInteger b ints)

      describe "negateInteger" $ do
        it "considers min bound Int" $
          shouldEqualHex (X.negateInteger $ X.smallInteger INT_MINBOUND#)
                         (Y.negateInteger $ Y.smallInteger INT_MINBOUND#)

      describe "shiftLInteger" $ do
        prop "works for random Int#" $ \(SmallInt (I# i), Positive (I# c#)) ->
          showHexX (X.shiftLInteger (X.smallInteger i) c#) === showHexY (Y.shiftLInteger (Y.smallInteger i) c#)

    describe "BigNum" $ do
      prop "wordToBigNum . bigNumToWord" $ \w@(W# w#) ->
        W# (X.bigNumToWord (X.wordToBigNum w#)) === w

      prop "wordToBigNum (w1 or w2) == (wordToBigNum w1) `orBigNum` (wordToBigNum w2)" $ \(W# w1, W# w2) ->
        show (X.wordToBigNum (w1 `or#` w2)) === show ((X.wordToBigNum w1) `X.orBigNum` (X.wordToBigNum w2))

      prop "minusBigNumWord (wordToBigNum w) w == wordToBigNum 0" $ \(W# w#) ->
        show (X.minusBigNumWord 0# (X.wordToBigNum w#) w#) === show (X.wordToBigNum 0##)

      -- it "can shift left" $ do
      --   print (X.shiftLBigNum (X.wordToBigNum 1##) 64#)

    -- it "Can create Integers" $ do
    --     shouldEqualHex (X.mkInteger True [0xbb, 0xaa])
    --                  (Y.mkInteger True [0xbb, 0xaa])
    --     shouldEqualHex (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
    --                  (Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
    -- describe "Low level" $
    --   it "Can add/mul" $ do
    --     a <- X.newBN
    --     X.setWord a 0x11##
    --     shouldBe ("0x" ++ X.bn2hex a) "0x11"
    --     b <- X.newBN
    --     X.setWord b 0x02##
    --     shouldBe ("0x" ++ X.bn2hex b) "0x02"
    --     r <- X.addBN a b
    --     shouldBe ("0x" ++ X.bn2hex r) "0x13"
    --     X.freeBN r
    --     r <- X.mulBN a b
    --     shouldBe ("0x" ++ X.bn2hex r) "0x22"
    --     X.freeBN r
    --     X.freeBN a
    --     X.freeBN b


showHexY :: Y.Integer -> String
showHexY i
  | i >= 0 = showHex i ""
  | otherwise = "-" ++ (showHex (Y.negateInteger i) "")

showHexX :: X.Integer -> String
showHexX i@(X.S# i#)
  | isTrue# (i# >=# 0#) = showHex (I# i#) ""
  | otherwise = "-" ++ (showHexX (X.negateInteger i))
showHexX (X.Bn# bn) = "-" ++ (show bn)
showHexX (X.Bp# bn) = show bn

-- TODO(SN): decimal show instance
instance Show X.Integer where
  show = showHexX

instance Show X.BigNum where
  -- Return base16 encoded string of underlying byte array, MSB first
  show (X.BN# ba#) =
    dropWhile (== '0') $ go (sizeofByteArray# ba# -# 1#)
   where
    go 0# = word8hex (indexWord8Array# ba# 0#)
    go i# = word8hex (indexWord8Array# ba# i#) ++ go (i# -# 1#)

    word8hex :: Word# -> String
    word8hex w
      | isTrue# (w `ltWord#` 0xA##) = ['0', wordToDigit w]
      | True = [wordToDigit ((w `uncheckedShiftRL#` 4#) `and#` 0xF##), wordToDigit (w `and#` 0xF##)]

    wordToDigit w = intToDigit $ I# (word2Int# w)

-- | HSpec expectation using hex representation
shouldEqualHex :: X.Integer -> Y.Integer -> Expectation
shouldEqualHex x y = showHexX x `shouldBe` showHexY y

-- | Newtype wrapper to generate minbound/maxbound values more often in
-- QuickCheck Arbitrary instance.
newtype SmallInt = SmallInt Int deriving Show

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> frequency [ (4, arbitrary)
                                     , (1, pure (I# INT_MINBOUND#))
                                     , (1, pure (I# INT_MAXBOUND#))
                                     ]

-- | Newtype wrapper to test various Integers via QuickCheck.
newtype Integers = Integers (X.Integer, Y.Integer) deriving Show

instance Testable Integers where
  property (Integers (x, y)) =
    counterexample (showHexX x ++ " /= " ++ showHexY y) (showHexX x == showHexY y)

instance Arbitrary Integers where
  arbitrary = oneof [small, big]
   where
    small = do
      (SmallInt (I# i)) <- arbitrary
      pure $ Integers (X.smallInteger i, Y.smallInteger i)

    big = do
      positive <- arbitrary
      ints <- map truncate32pos <$> arbitrary -- 31bit int chunks
      pure $ Integers (X.mkInteger positive ints, Y.mkInteger positive ints)

-- Truncate to a positive 32bit integer (required for mkInteger)
truncate32pos :: Int -> Int
truncate32pos i = abs i .&. 0x7fffffff
