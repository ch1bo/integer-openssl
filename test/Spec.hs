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

main :: IO ()
main = hspec $ do
  -- putStrLn ""
  -- putStrLn . showHexX $ X.Bn# (X.wordToBigNum 0xabb##)
  -- putStrLn $ showHexY (-0xabb :: Y.Integer)
  describe "smallInteger" $ do
    prop "can convert from random Int#" $ \(I# i) ->
      showHexX (X.smallInteger i) === showHexY (Y.smallInteger i)
  describe "mkInteger" $ do
    it "can create Integers" $ do
        shouldEqualHex (X.mkInteger True [0xbb, 0xaa])
                       (Y.mkInteger True [0xbb, 0xaa])
        shouldEqualHex (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
                       (Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
    prop "can create random Integers" $ \(b, is) ->
      let ints = map truncate32pos is
      in  showHexX (X.mkInteger b ints) === showHexY (Y.mkInteger b ints)

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
  | otherwise = "-" ++ (showHex (abs i) "")

showHexX :: X.Integer -> String
showHexX (X.S# i)
  | isTrue# (i >=# 0#) = showHex (I# i) ""
  | otherwise = "-" ++ (showHex (abs (I# i)) "")
showHexX (X.Bn# bn) = "-" ++ (show bn)
showHexX (X.Bp# bn) = show bn

-- TODO(SN): decimal show instance
instance Show X.Integer where
  show = showHexX

instance Show X.BigNum where
  -- Return base16 encoded string of underlying byte array, MSB first
  show (X.BN# ba) =
    dropWhile (== '0') $ go (sizeofByteArray# ba -# 1#)
   where
    go 0# = word8hex (indexWord8Array# ba 0#)
    go i# = word8hex (indexWord8Array# ba i#) ++ go (i# -# 1#)

    word8hex :: Word# -> String
    word8hex w
      | isTrue# (w `ltWord#` 0xA##) = [wordToDigit w]
      | True = [wordToDigit ((w `uncheckedShiftRL#` 4#) `and#` 0xF##), wordToDigit (w `and#` 0xF##)]

    wordToDigit w = intToDigit $ I# (word2Int# w)

-- | HSpec expectation using hex representation
shouldEqualHex :: X.Integer -> Y.Integer -> Expectation
shouldEqualHex x y = showHexX x `shouldBe` showHexY y

newtype Integers = Integers (X.Integer, Y.Integer) deriving Show

instance Testable Integers where
  property (Integers (x, y)) =
    counterexample (showHexX x ++ " /= " ++ showHexY y) (showHexX x == showHexY y)

instance Arbitrary Integers where
  arbitrary = oneof [small, big]

small :: Gen Integers
small = do
  (I# i) <- arbitrary
  pure $ Integers (X.smallInteger i, Y.smallInteger i)

big :: Gen Integers
big = do
  positive <- arbitrary
  ints <- map truncate32pos <$> arbitrary -- 31bit int chunks
  pure $ Integers (X.mkInteger positive ints, Y.mkInteger positive ints)

-- Truncate to a positive 32bit integer (required for mkInteger)
truncate32pos :: Int -> Int
truncate32pos i = abs i .&. 0x7fffffff
