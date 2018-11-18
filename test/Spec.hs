{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE PackageImports      #-}

module Main where

import           Data.Bits             ((.&.))
import           Data.Char             (intToDigit)
import           GHC.Prim
import           GHC.Types
import           Numeric               (showHex)
import           Test.Hspec            (Expectation, describe, hspec, it,
                                        shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       hiding ((.&.), NonZero)

import "integer-gmp"  GHC.Integer      as Y
import "integer-openssl" GHC.Integer.Type   as X hiding (($))


#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
# define INT_MINBOUND      -0x8000000000000000
# define INT_MAXBOUND       0x7fffffffffffffff
# define ABS_INT_MINBOUND   0x8000000000000000
# define WORD_SIZE_IN_BYTES 8
# define WORD_SHIFT         3
# define WORD_MINBOUND      0x0000000000000000
# define WORD_MAXBOUND      0xffffffffffffffff
#elif WORD_SIZE_IN_BITS == 32
# define INT_MINBOUND       -0x80000000
# define INT_MAXBOUND       0x7fffffff
# define ABS_INT_MINBOUND   0x80000000
# define WORD_SIZE_IN_BYTES 4
# define WORD_SHIFT         2
# define WORD_MINBOUND       0x00000000
# define WORD_MAXBOUND       0xffffffff
#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

main :: IO ()
main = do

  let --vals = [40000000, 20000000, 4000000, 180000005]
      vals = [3, 1]
      x = X.mkInteger False vals 
      y = Y.mkInteger False vals
  
      w = 5##
      i = -5#
 
  -- putStrLn $ "\nencodeDouble#: 5 " <> show (D# (X.encodeDoubleInteger x -2#))
  --   <> "\nencodeDouble#: -5 " <> show (D# (encodeDouble# (int2Word# i) -2#))
 
  putStrLn $ "\nX.encodeDoubleInteger: " <> showHexX x <> " = " <> show (D# (X.encodeDoubleInteger x -1#))
  putStrLn $ "Y.encodeDoubleInteger: " <> showHexY y <> " = " <> show (D# (Y.encodeDoubleInteger y -1#))

  hspec $ do
    describe "library vs builtin" $ do
      describe "smallIntger" $ do
        prop "works for random Int#" $ \(SmallInt (I# i)) ->
          X.smallInteger i <<>> Y.smallInteger i

      describe "mkInteger" $ do
        it "can create some Integers" $ do
          shouldEqualHex (X.mkInteger True [0xbb, 0xaa])
                         (Y.mkInteger True [0xbb, 0xaa])
          shouldEqualHex (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
                         (Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
        prop "can create random Integers" $ \(b, is) ->
          let ints = map truncate32pos is
          in  X.mkInteger b ints <<>> Y.mkInteger b ints

      describe "timesInteger" $ do
        prop "can multiply random Integers" $ \((Integers x1 y1), (Integers x2 y2)) ->
          X.timesInteger x1 x2 <<>> Y.timesInteger y1 y2

      describe "quotRemInteger" $ do
        -- division by zero cannot be tested properly here
        prop "can divide random Integers" $ \((Integers x1 y1), NonZero (Integers x2 y2)) ->
          let (# xq, qr #) = X.quotRemInteger x1 x2
              (# yq, yr #) = Y.quotRemInteger y1 y2
          in xq <<>> yq

      describe "negateInteger" $ do
        it "considers min bound Int" $
          shouldEqualHex (X.negateInteger $ X.smallInteger INT_MINBOUND#)
                         (Y.negateInteger $ Y.smallInteger INT_MINBOUND#)

      describe "shiftLInteger" $ do
        prop "works for random Int#" $ \(SmallInt (I# i), Positive (I# c#)) ->
          X.shiftLInteger (X.smallInteger i) c# <<>> Y.shiftLInteger (Y.smallInteger i) c#

      describe "wordToInteger" $ do
        prop "works for random Word#" $ \(Positive (W# c#)) ->
          X.wordToInteger c# <<>> Y.wordToInteger c#

      describe "integerToWord" $ do
        prop "works for random Integer" $ \(Integers x1 y1) ->
          isTrue# (eqWord# (X.integerToWord x1) (Y.integerToWord y1))

      describe "integerToInt" $ do
        prop "works for random Integer" $ \(Integers x1 y1) ->
          isTrue# (X.integerToInt x1 ==# Y.integerToInt y1)

      describe "doubleFromInteger" $ do
        prop "works for random Integer" $ \(Integers x1 y1) ->
          isTrue# (X.doubleFromInteger x1 ==## Y.doubleFromInteger y1)

      describe "encodeDoubleInteger" $ do
        prop "works for random Integer" $ \(Integers x1 y1, SmallInt (I# i)) ->
          isTrue# (X.encodeDoubleInteger x1 i ==## Y.encodeDoubleInteger y1 i)
    
    -- describe "BigNum" $ do
    --   prop "wordToBigNum . bigNumToWord" $ \w@(W# w#) ->
    --     W# (X.bigNumToWord (X.wordToBigNum w#)) === w

    --   prop "wordToBigNum (w1 or w2) == (wordToBigNum w1) `orBigNum` (wordToBigNum w2)" $ \(W# w1, W# w2) ->
    --     show (X.wordToBigNum (w1 `or#` w2)) === show ((X.wordToBigNum w1) `X.orBigNum` (X.wordToBigNum w2))

    --   prop "minusBigNumWord (wordToBigNum w) w == wordToBigNum 0" $ \(W# w#) ->
    --     show (X.minusBigNumWord 0# (X.wordToBigNum w#) w#) === show (X.wordToBigNum 0##)

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

(<<>>) :: X.Integer -> Y.Integer -> Property
x <<>> y =
  counterexample (showHexX x ++ " /= " ++ showHexY y) (showHexX x == showHexY y)

-- | Newtype to generate minbound/maxbound values more often in QuickCheck
-- Arbitrary instance.
newtype SmallInt = SmallInt Int deriving Show

instance Arbitrary SmallInt where
  arbitrary = SmallInt <$> frequency [ (4, arbitrary)
                                     , (1, pure 0)
                                     , (1, pure (I# INT_MINBOUND#))
                                     , (1, pure (I# INT_MAXBOUND#))
                                     ]


-- | Datatype to test various Integers via QuickCheck.
data Integers = Integers X.Integer Y.Integer deriving Show

instance Arbitrary Integers where
  arbitrary = oneof [small, big]
   where
    small = do
      (SmallInt (I# i)) <- arbitrary
      pure $ Integers (X.smallInteger i) (Y.smallInteger i)

    big = do
      positive <- arbitrary
      ints <- map truncate32pos <$> arbitrary -- 31bit int chunks
      pure $ Integers (X.mkInteger positive ints) (Y.mkInteger positive ints)

-- | Newtype to generate non-zero integer Arbitrary instance.
newtype NonZeroIntegers = NonZero Integers deriving Show

instance Arbitrary NonZeroIntegers where
  arbitrary = NonZero <$> arbitrary `suchThat` notZero
   where
    notZero (Integers (X.S# 0#) _) = False
    notZero (Integers _ y) = y /= 0

-- Truncate to a positive 32bit integer (required for mkInteger)
truncate32pos :: Int -> Int
truncate32pos i = abs i .&. 0x7fffffff
