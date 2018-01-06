import Test.Hspec (hspec, describe, it, shouldBe)

import GHC.Types

import qualified OpenSSL.GHC.Integer as X -- this library
import qualified GHC.Integer as Y -- builtin, usually integer-gmp

main :: IO ()
main = hspec $ do
    describe "mkInteger" $ do
      it "Can create Integers" $ do
        a <- X.newBN
        X.setWord a 0x11
        putStrLn $ "0x" ++ X.bn2hex a
        b <- X.newBN
        X.setWord b 0x02
        putStrLn $ "0x" ++ X.bn2hex b
        r <- X.mulBN a b
        putStrLn $ "0x" ++ X.bn2hex r
        X.freeBN a
        X.freeBN b
        X.freeBN r
        -- print $ X.mkInteger True [0xff]
        -- print $ X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f]
        -- print $ Y.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f]

-- * Internal functions

instance Show X.Integer where
  show (X.S# i) = show $ I# i
  show _ = "not implemented" -- TODO(SN): not implemented
