import Test.Hspec (hspec, describe, it, shouldBe, Expectation)
import GHC.Types

import qualified OpenSSL.GHC.Integer as X -- this library
import qualified GHC.Integer as Y -- builtin, usually integer-gmp

main :: IO ()
main = hspec $ do
    describe "mkInteger" $ do
      it "Can create Integers" $ do
        shouldEqualS (X.mkInteger True [0xbb, 0xaa])
                     (Y.mkInteger True [0xbb, 0xaa])
        shouldEqualS (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
                     (Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f])
    describe "Low level" $
      it "Can add/mul" $ do
        a <- X.newBN
        X.setWord a 0x11##
        shouldBe ("0x" ++ X.bn2hex a) "0x11"
        b <- X.newBN
        X.setWord b 0x02##
        shouldBe ("0x" ++ X.bn2hex b) "0x02"
        r <- X.addBN a b
        shouldBe ("0x" ++ X.bn2hex r) "0x13"
        X.freeBN r
        r <- X.mulBN a b
        shouldBe ("0x" ++ X.bn2hex r) "0x22"
        X.freeBN r
        X.freeBN a
        X.freeBN b

shouldEqualS :: (Show a, Show b) => a -> b -> Expectation
shouldEqualS a b = show a `shouldBe` show b

-- * Internal functions

instance Show X.Integer where
  show (X.S# i) = show $ I# i
  show (X.B# bn) = X.bn2dec bn
