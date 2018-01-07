{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ExplicitForAll         #-}
module GHC.Integer where

#include "MachDeps.h"

import GHC.CString
import GHC.Magic
import GHC.Prim
import GHC.Types

-- | Opaque OpenSsl BIGNUM
data BigNum = BN# Addr#

data BigNumCtx = CTX# Addr#

data Integer = S# !Int#
             | B# {-# UNPACK #-} !BigNum

-- | Construct 'Integer' value from list of 'Int's.
             --
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least
                    -- significant first
          -> Integer
mkInteger _ [I# i] = S# (i `andI#` 0x7FFFFFFF#)
mkInteger _ is = runS . liftIO $ do
  bn <- newBN
  go bn is
  return $ B# bn
 where
  go bn [(I# i)] = setWord bn (int2Word# (i `andI#` 0x7FFFFFFF#))
  go bn ((I# i):xs) = do
    go bn xs
    lshift bn 31#
    setWord bn (int2Word# (i `andI#` 0x7FFFFFFF#))
{-# NOINLINE mkInteger #-}

smallInteger :: Int# -> Integer
smallInteger i# = S# i#
{-# NOINLINE smallInteger #-}

-- * Internal functions

newBN :: IO BigNum
newBN = do
  (W# w) <- bn_new
  return $ BN# (unsafeCoerce# w)

-- TODO(SN): @IO Word@ as @IO Addr#@ and @(# State# RealWorld, Addr# #)@ not allowed
-- BIGNUM *BN_new(void)
foreign import ccall unsafe "BN_new" bn_new :: IO Word

freeBN :: BigNum -> IO ()
freeBN (BN# addr) = bn_free addr

-- void BN_free(BIGNUM *a);
foreign import ccall unsafe "BN_free" bn_free :: Addr# -> IO ()

newCtx :: IO BigNumCtx
newCtx = do
  (W# w) <- bn_ctx_new
  return $ CTX# (unsafeCoerce# w)

-- BN_CTX *BN_CTX_new(void);
foreign import ccall unsafe "BN_CTX_new" bn_ctx_new :: IO Word

freeCtx :: BigNumCtx -> IO ()
freeCtx (CTX# addr) = bn_ctx_free addr

-- void BN_CTX_free(BN_CTX *c);
foreign import ccall unsafe "BN_CTX_free" bn_ctx_free :: Addr# -> IO ()

bn2dec :: BigNum -> [Char]
bn2dec (BN# addr) = unpackCString# (bn_bn2dec addr)

-- char *BN_bn2dec(const BIGNUM *a);
foreign import ccall unsafe "BN_bn2dec" bn_bn2dec :: Addr# -> Addr#

bn2hex :: BigNum -> [Char]
bn2hex (BN# addr) = unpackCString# (bn_bn2hex addr)

-- char *BN_bn2hex(const BIGNUM *a);
foreign import ccall unsafe "BN_bn2hex" bn_bn2hex :: Addr# -> Addr#

setWord :: BigNum -> Word# -> IO ()
setWord (BN# addr) w = do
  x <- bn_set_word addr w
  case x of
    1 -> return ()
    _ -> IO $ fail "BN_set_word failed"

-- int BN_set_word(BIGNUM *a, BN_ULONG w);
foreign import ccall unsafe "BN_set_word" bn_set_word :: Addr# -> Word# -> IO Int

lshift :: BigNum -> Int# -> IO ()
lshift (BN# a) n
  | isTrue# (n ==# 0#) = return ()
  | isTrue# (n ># 0#) = do
      x <- bn_lshift a a n
      case x of
        1 -> return ()
        _ -> IO $ fail "BN_lshift failed"
  | isTrue# (n <# 0#) = IO $ fail "BN_lshift negative n"

-- int BN_lshift(BIGNUM *r, const BIGNUM *a, int n);
foreign import ccall unsafe "BN_lshift" bn_lshift :: Addr# -> Addr# -> Int# -> IO Int

addBN :: BigNum -> BigNum -> IO BigNum
addBN (BN# a) (BN# b) = do
  (BN# r) <- newBN
  x <- bn_add r a b
  case x of
    1 -> return $ BN# r
    _ -> runS $ fail "BN_add failed"

-- int BN_add(BIGNUM *r, const BIGNUM *a, const BIGNUM *b);
foreign import ccall unsafe "BN_add" bn_add :: Addr# -> Addr# -> Addr# -> IO Int

mulBN :: BigNum -> BigNum -> IO BigNum
mulBN (BN# a) (BN# b) = do
  ctx@(CTX# c) <- newCtx
  (BN# r) <- newBN
  x <- bn_mul r a b c
  freeCtx ctx
  case x of
    1 -> return $ BN# r
    _ -> runS $ fail "BN_mul failed"

-- int BN_mul(BIGNUM *r, const BIGNUM *a, const BIGNUM *b, BN_CTX *ctx);
foreign import ccall unsafe "BN_mul" bn_mul :: Addr# -> Addr# -> Addr# -> Addr# -> IO Int

-- Foreign:

type role Ptr phantom
data Ptr a = Ptr Addr#

-- TODO(SN): add a managed Ptr to free on garbage collect (ForeignPtr)

-- From integer-gmp:
-- monadic combinators for low-level state threading

type S s a = State# s -> (# State# s, a #)

infixl 1 >>=
infixl 1 >>
infixr 0 $

{-# INLINE (.) #-}
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

{-# INLINE ($) #-}
($) :: (a -> b) -> a -> b
f $ x = f x

{-# INLINE (>>=) #-}
(>>=) :: S s a -> (a -> S s b) -> S s b
(>>=) m k = \s -> case m s of (# s', a #) -> k a s'

{-# INLINE (>>) #-}
(>>) :: S s a -> S s b -> S s b
(>>) m k = \s -> case m s of (# s', _ #) -> k s'

{-# INLINE svoid #-}
svoid :: (State# s -> State# s) -> S s ()
svoid m0 = \s -> case m0 s of s' -> (# s', () #)

{-# INLINE return #-}
return :: a -> IO a
return a = IO $ \s -> (# s, a #)

{-# INLINE return# #-}
return# :: a -> S s a
return# a = \s -> (# s, a #)

{-# INLINE liftIO #-}
liftIO :: IO a -> S RealWorld a
liftIO (IO m) = m

-- NB: equivalent of GHC.IO.unsafeDupablePerformIO, see notes there
runS :: S RealWorld a -> a
runS m = case runRW# m of (# _, a #) -> a

-- stupid hack
fail :: [Char] -> S s a
fail s = return# (raise# s)

-- GHC.Err:

undefined :: forall a. a
undefined = runS $ fail "Prelude.undefined"
