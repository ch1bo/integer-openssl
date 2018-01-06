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
mkInteger _ [I# i] = S# i
mkInteger _ _      = S# 0#
{-# NOINLINE mkInteger #-}

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

bn2hex :: BigNum -> [Char]
bn2hex (BN# addr) = unpackCString# (bn_bn2hex addr)

-- char *BN_bn2hex(const BIGNUM *a);
foreign import ccall unsafe "BN_bn2hex" bn_bn2hex :: Addr# -> Addr#

setWord :: BigNum -> Word -> IO ()
setWord (BN# addr) (W# w) = do
  x <- bn_set_word addr w
  case x of
    1 -> return ()
    _ -> IO $ fail "BN_set_word failed"

-- int BN_set_word(BIGNUM *a, BN_ULONG w);
foreign import ccall unsafe "BN_set_word" bn_set_word :: Addr# -> Word# -> IO Int

mulBN :: BigNum -> BigNum -> IO BigNum
mulBN (BN# a) (BN# b) = do
  ctx@(CTX# c) <- newCtx
  (BN# r) <- newBN
  x <- bn_mul r a b c
  freeCtx ctx
  case x of
    0 -> return $ BN# r
    _ -> runS $ fail "BN_mul failed"

-- int BN_mul(BIGNUM *r, const BIGNUM *a, const BIGNUM *b, BN_CTX *ctx);
foreign import ccall unsafe "BN_mul" bn_mul :: Addr# -> Addr# -> Addr# -> Addr# -> IO Int


-- newBigNum :: Int# -> S s BigNum
-- newBigNum n s =
--   let (# s', mba #) = newByteArray# n s
--       (# s'', ba #) = unsafeFreezeByteArray# mba s'
--   in  (# s'', BN# ba #)


-- Foreign.Storable:

-- class Storable a where
--    {-# MINIMAL sizeOf, alignment,
--                (peek | peekElemOff | peekByteOff),
--                (poke | pokeElemOff | pokeByteOff) #-}

--    sizeOf      :: a -> Int
--    -- ^ Computes the storage requirements (in bytes) of the argument.
--    -- The value of the argument is not used.

--    alignment   :: a -> Int
--    -- ^ Computes the alignment constraint of the argument.  An
--    -- alignment constraint @x@ is fulfilled by any address divisible
--    -- by @x@.  The value of the argument is not used.

--    peekElemOff :: Ptr a -> Int      -> IO a
--    -- ^       Read a value from a memory area regarded as an array
--    --         of values of the same kind.  The first argument specifies
--    --         the start address of the array and the second the index into
--    --         the array (the first element of the array has index
--    --         @0@).  The following equality holds,
--    -- 
--    -- > peekElemOff addr idx = IOExts.fixIO $ \result ->
--    -- >   peek (addr `plusPtr` (idx * sizeOf result))
--    --
--    --         Note that this is only a specification, not
--    --         necessarily the concrete implementation of the
--    --         function.

--    pokeElemOff :: Ptr a -> Int -> a -> IO ()
--    -- ^       Write a value to a memory area regarded as an array of
--    --         values of the same kind.  The following equality holds:
--    -- 
--    -- > pokeElemOff addr idx x = 
--    -- >   poke (addr `plusPtr` (idx * sizeOf x)) x

--    peekByteOff :: Ptr b -> Int      -> IO a
--    -- ^       Read a value from a memory location given by a base
--    --         address and offset.  The following equality holds:
--    --
--    -- > peekByteOff addr off = peek (addr `plusPtr` off)

--    pokeByteOff :: Ptr b -> Int -> a -> IO ()
--    -- ^       Write a value to a memory location given by a base
--    --         address and offset.  The following equality holds:
--    --
--    -- > pokeByteOff addr off x = poke (addr `plusPtr` off) x
  
--    peek        :: Ptr a      -> IO a
--    -- ^ Read a value from the given memory location.
--    --
--    --  Note that the peek and poke functions might require properly
--    --  aligned addresses to function correctly.  This is architecture
--    --  dependent; thus, portable code should ensure that when peeking or
--    --  poking values of some type @a@, the alignment
--    --  constraint for @a@, as given by the function
--    --  'alignment' is fulfilled.

--    poke        :: Ptr a -> a -> IO ()
--    -- ^ Write the given value to the given memory location.  Alignment
--    -- restrictions might apply; see 'peek'.
 
--    -- circular default instances
--    peekElemOff = peekElemOff_ undefined
--       where peekElemOff_ :: Storable a => a -> Ptr a -> Int -> IO a
--             peekElemOff_ undef ptr (I# off) =
--               let (I# s) = sizeOf undef
--               in peekByteOff ptr (I# (off *# s))
--    pokeElemOff ptr (I# off) val =
--      let (I# v) = sizeOf val
--      in pokeByteOff ptr (I# (off *# v)) val

--    peekByteOff (Ptr ptr) (I# off) = peek $ Ptr (ptr `plusAddr#` off)
--    pokeByteOff (Ptr ptr) (I# off) = poke $ Ptr (ptr `plusAddr#` off)

--    peek ptr = peekElemOff ptr 0
--    poke ptr = pokeElemOff ptr 0

-- -- | @since 4.9.0.0
-- instance Storable () where
--   sizeOf _ = 0
--   alignment _ = 1
--   peek _ = IO $ return ()
--   poke _ _ = IO $ return ()

-- instance Storable (Ptr a) where
--   sizeOf _ = SIZEOF_HSPTR
--   alignment _ = ALIGNMENT_HSPTR
--   peekElemOff (Ptr a) (I# i) =
--     IO $ \s -> case readAddrOffAddr# a i s of (# s2, x #) -> (# s2, Ptr x #)
--   pokeElemOff (Ptr a) (I# i) (Ptr x) =
--     IO $ \s -> case writeAddrOffAddr# a i x s      of s2 -> (# s2, () #)

-- Foreign:

type role Ptr phantom
data Ptr a = Ptr Addr#

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
