{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE RebindableSyntax         #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
module GHC.Integer where

#include "MachDeps.h"

import GHC.Magic
import GHC.Prim
import GHC.Types

wordSize :: Int
wordSize = I# WORD_SIZE_IN_BITS#

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

-- TODO(SN): general:
--  - add short cuts
--  - look into lazyness (bang patterns)

-- | OpenSSL BIGNUM represented by an absolute magnitude as 'Word#' in a
-- 'ByteArray#'. It corresponds to the 'd' array in libcrypto's bignum_st
-- structure. Length is always a multiple of Word#, least-significant first
-- (BN_BITS2 == WORD_SIZE_IN_BITS).
data BigNum = BN# ByteArray#

-- | Mutable variant of BigNum for internal use.
data MutableBigNum s = MBN# (MutableByteArray# s)

data Integer = S# !Int#
               -- ^ small integer
             | Bp# {-# UNPACK #-} !BigNum
               -- ^ positive bignum, > maxbound(Int)
             | Bn# {-# UNPACK #-} !BigNum
               -- ^ negative bignum, < minbound(Int)

-- | Construct 'Integer' value from list of 'Int's.
             --
-- This function is used by GHC for constructing 'Integer' literals.
mkInteger :: Bool   -- ^ sign of integer ('True' if non-negative)
          -> [Int]  -- ^ absolute value expressed in 31 bit chunks, least
                    -- significant first
          -> Integer
mkInteger nonNegative is
  | nonNegative = f is
  | True = negateInteger (f is)
 where
  f [] = S# 0#
  f (I# i : is') = smallInteger (i `andI#` 0x7fffffff#) `orInteger` shiftLInteger (f is') 31#
{-# NOINLINE mkInteger #-}

-- | Create an Integer from a single Int#.
smallInteger :: Int# -> Integer
smallInteger i# = S# i#
{-# NOINLINE smallInteger #-}

-- | Create a (positive) Integer from a single Word#.
wordToInteger :: Word# -> Integer
wordToInteger w#
  | isTrue# (i# >=# 0#) = S# i#
  | True = Bp# (wordToBigNum w#)
  where
    i# = word2Int# w#
{-# NOINLINE wordToInteger #-}

-- | Create a (negative) Integer from a single Word#.
wordToNegInteger :: Word# -> Integer
wordToNegInteger w#
  | isTrue# (i# <=# 0#) = S# i#
  | True = Bn# (wordToBigNum w#)
  where
    i# = negateInt# (word2Int# w#)
-- inlinable as only internally used

-- | Integer multiplication.
timesInteger :: Integer -> Integer -> Integer
timesInteger _ (S# 0#) = S# 0#
timesInteger (S# 0#) _ = S# 0#
timesInteger x (S# 1#) = x
timesInteger (S# 1#) y = y
timesInteger x (S# -1#) = negateInteger x
timesInteger (S# -1#) y = negateInteger y
timesInteger (S# x#) (S# y#) =
  case mulIntMayOflo# x# y# of
    0# -> S# (x# *# y#)
    _  -> timesInt2Integer x# y#
timesInteger x@(S# _) y = timesInteger y x
timesInteger (Bp# x) (Bp# y) = Bp# (timesBigNum x y)
timesInteger (Bp# x) (Bn# y) = Bn# (timesBigNum x y)
timesInteger (Bp# x) (S# y#)
  | isTrue# (y# >=# 0#) = Bp# (timesBigNumWord x (int2Word# y#))
  | True = Bn# (timesBigNumWord x (int2Word# (negateInt# y#)))
timesInteger (Bn# x) (Bn# y) = Bp# (timesBigNum x y)
timesInteger (Bn# x) (Bp# y) = Bn# (timesBigNum x y)
timesInteger (Bn# x) (S# y#)
  | isTrue# (y# >=# 0#) =  Bn# (timesBigNumWord x (int2Word# y#))
  | True = Bp# (timesBigNumWord x (int2Word# (negateInt# y#)))

-- | Construct 'Integer' from the product of two 'Int#'s
timesInt2Integer :: Int# -> Int# -> Integer
timesInt2Integer x# y# =
  case (# isTrue# (x# >=# 0#), isTrue# (y# >=# 0#) #) of
    (# False, False #) -> case timesWord2# (int2Word# (negateInt# x#)) (int2Word# (negateInt# y#)) of
      (# 0##, l #) -> inline wordToInteger l
      (# h, l #) -> Bp# (wordToBigNum2 h l)

    (# True, False #) -> case timesWord2# (int2Word# x#) (int2Word# (negateInt# y#)) of
      (# 0##, l #) -> wordToNegInteger l
      (# h, l #) -> Bn# (wordToBigNum2 h l)

    (# False, True #) -> case timesWord2# (int2Word# (negateInt# x#)) (int2Word# y#) of
      (# 0##, l #) -> wordToNegInteger l
      (# h, l #) -> Bn# (wordToBigNum2 h l)

    (# True, True #) -> case timesWord2# (int2Word# x#) (int2Word# y#) of
      (# 0##, l #) -> inline wordToInteger l
      (# h, l #) -> Bp# (wordToBigNum2 h l)

-- | Integer division rounded to zero, calculating 'quotInteger' and
-- 'remInteger'. Divisor must be non-zero or a division-by-zero will be raised.
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger n (S# 1#) = (# n, S# 0# #)
quotRemInteger n (S# -1#) =
  let !q = negateInteger n
  in  (# q, S# 0# #)
quotRemInteger _ (S# 0#) = -- will raise division by zero
  (# S# (quotInt# 0# 0#), S# (remInt# 0# 0#) #)
quotRemInteger (S# 0#) _ = (# S# 0#, S# 0# #)
quotRemInteger (S# n#) (S# d#) =
  let (# q#, r# #) = quotRemInt# n# d#
  in  (# S# q#, S# r# #)
quotRemInteger (Bp# n) (Bp# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToInteger q, bigNumToInteger r #)
quotRemInteger (Bp# n) (Bn# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToNegInteger q, bigNumToInteger r #)
quotRemInteger (Bn# n) (Bn# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToInteger q, bigNumToNegInteger r #)
quotRemInteger (Bn# n) (Bp# d) =
  let (# q, r #) = quotRemBigNum n d
  in  (# bigNumToNegInteger q, bigNumToNegInteger r #)
quotRemInteger (Bp# n) (S# d#)
  | isTrue# (d# >=# 0#) = let (# q, r# #) = quotRemBigNumWord n (int2Word# d#)
                          in  (# bigNumToInteger q, inline wordToInteger r# #)
  | True = let (# q, r# #) = quotRemBigNumWord n (int2Word# (negateInt# d#))
           in  (# bigNumToNegInteger q, inline wordToInteger r# #)
quotRemInteger (Bn# n) (S# d#)
  | isTrue# (d# >=# 0#) = let (# q, r# #) = quotRemBigNumWord n (int2Word# d#)
                          in  (# bigNumToNegInteger q, inline wordToNegInteger r# #)
  | True = let (# q, r# #) = quotRemBigNumWord n (int2Word# (negateInt# d#))
           in  (# bigNumToInteger q, inline wordToNegInteger r# #)
quotRemInteger i@(S# _) (Bn# d) = (# S# 0#, i #) -- since i < d
quotRemInteger i@(S# i#) (Bp# d) -- need to account for (S# minBound)
    | isTrue# (i# ># 0#) = (# S# 0#, i #)
    | isTrue# (gtBigNumWord# d (int2Word# (negateInt# i#))) = (# S# 0#, i #)
    | True {- abs(i) == d -} = (# S# -1#, S# 0# #)
{-# NOINLINE quotRemInteger #-}

-- | Switch sign of Integer.
negateInteger :: Integer -> Integer
negateInteger (Bn# n) = Bp# n
negateInteger (S# INT_MINBOUND#) = Bp# (wordToBigNum ABS_INT_MINBOUND##)
negateInteger (S# i#) = S# (negateInt# i#)
negateInteger (Bp# bn)
  | isTrue# (eqBigNumWord# bn ABS_INT_MINBOUND##) = S# INT_MINBOUND#
  | True = Bn# bn
{-# NOINLINE negateInteger #-}

-- | Bitwise OR of Integers.
orInteger :: Integer -> Integer -> Integer
-- short-cuts
orInteger (S# 0#) y = y
orInteger x (S# 0#) = x
orInteger x@(S# -1#) _ = x
orInteger _ y@(S# -1#) = y
-- base-cases
orInteger (S# a#) (S# b#) = S# (a# `orI#` b#)
orInteger (Bp# x) (Bp# y) = Bp# (orBigNum x y)
orInteger (Bn# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord 1# (andBigNum (minusBigNumWord 1# x 1##)
                                                   (minusBigNumWord 1# y 1##)) 1##)
orInteger x@(Bn# _) y@(Bp# _) = orInteger y x -- swap for next case
orInteger (Bp# x) (Bn# y) =
  bigNumToNegInteger (plusBigNumWord 1# (andnBigNum (minusBigNumWord 1# y 1##) x) 1##)
-- -- TODO/FIXpromotion-hack
orInteger  x@(S# _) y = orInteger (unsafePromote x) y
orInteger  x  y@(S# _) = orInteger x (unsafePromote y)
{-# NOINLINE orInteger #-}

-- HACK warning! breaks invariant on purpose
unsafePromote :: Integer -> Integer
unsafePromote (S# x#)
    | isTrue# (x# >=# 0#) = Bp# (wordToBigNum (int2Word# x#))
    | True                = Bn# (wordToBigNum (int2Word# (negateInt# x#)))
unsafePromote x = x

-- | Shift-left operation. Undefined for negative shift amount ('Int#').
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger x 0# = x
shiftLInteger (S# 0#) _  = S# 0#
-- TODO re-enable shiftLInteger (S# 1#) n# = bitInteger n#
shiftLInteger (S# i#) n#
  | isTrue# (i# >=# 0#) = bigNumToInteger (shiftLBigNum (wordToBigNum (int2Word# i#)) n#)
  | True = bigNumToNegInteger (shiftLBigNum (wordToBigNum (int2Word# (negateInt# i#))) n#)
shiftLInteger (Bp# bn) n# = Bp# (shiftLBigNum bn n#)
shiftLInteger (Bn# bn) n# = Bn# (shiftLBigNum bn n#)
{-# NOINLINE shiftLInteger #-}

-- * Functions operating on BigNum

zeroBigNum :: BigNum
zeroBigNum = runS (newBigNum 1# >>= freezeBigNum)

-- | Create a BigNum from a single Word#.
wordToBigNum :: Word# -> BigNum
wordToBigNum w# = runS $ do
  mbn <- newBigNum 1#
  writeBigNum mbn 0# w#
  freezeBigNum mbn

-- | Create a BigNum from two Word#.
wordToBigNum2 :: Word# -- ^ High word
              -> Word# -- ^ low word
              -> BigNum
wordToBigNum2 h# l# = runS $ do
  mbn <- newBigNum 2#
  writeBigNum mbn 0# l#
  writeBigNum mbn 1# h#
  freezeBigNum mbn

-- | Truncate a BigNum to a single Word#.
bigNumToWord :: BigNum -> Word#
bigNumToWord (BN# ba) = indexWordArray# ba 0#

-- | Create a positive Integer from given BigNum. Converts to small Integer if possible.
bigNumToInteger :: BigNum -> Integer
bigNumToInteger bn
  | isTrue# ((wordsInBigNum# bn ==# 1#) `andI#` (i# >=# 0#)) = S# i#
  | True = Bp# bn
  where
    i# = word2Int# (bigNumToWord bn)

-- | Create a negative Integer from given BigNum. Converts to small Integer if possible.
bigNumToNegInteger :: BigNum -> Integer
bigNumToNegInteger bn
  | isTrue# ((wordsInBigNum# bn ==# 1#) `andI#` (i# <=# 0#)) = S# i#
  | True = Bn# bn
  where
    i# = negateInt# (word2Int# (bigNumToWord bn))

-- ** Comparisons

-- | Get number of Word# in BigNum. See newBigNum for shift explanation.
wordsInBigNum# :: BigNum -> Int#
wordsInBigNum# (BN# ba#) = (sizeofByteArray# ba#) `uncheckedIShiftRL#` WORD_SHIFT#

-- | Return 1# iff BigNum holds one Word# equal to given Word#.
eqBigNumWord# :: BigNum -> Word# -> Int#
eqBigNumWord# bn w# =
  (wordsInBigNum# bn ==# 1#) `andI#` (bigNumToWord bn `eqWord#` w#)

-- | Return 1# iff BigNum is greater than a given Word#.
gtBigNumWord# :: BigNum -> Word# -> Int#
gtBigNumWord# bn w# =
  (wordsInBigNum# bn ># 1#) `orI#` (bigNumToWord bn `gtWord#` w#)

-- ** Bit-operations

-- | Bitwise OR of two BigNum.
orBigNum :: BigNum -> BigNum -> BigNum
orBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = y
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = orBigNum' x# y# nx# ny#
  | True = orBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  orBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    mapWordArray# a# b# mba# or# m#
    case isTrue# (n# ==# m#) of
      False -> copyWordArray# a# m# mba# m# (n# -# m#)
      True  -> return ()
    freezeBigNum mbn

-- | Bitwise AND of two BigNum.
andBigNum :: BigNum -> BigNum -> BigNum
andBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
  | isTrue# (eqBigNumWord# y 0##) = zeroBigNum
  | isTrue# (nx# >=# ny#) = andBigNum' x# y# nx# ny#
  | True = andBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m#
  andBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    mapWordArray# a# b# mba# and# m#
    freezeBigNum mbn -- TODO(SN): resize mbn if possible

-- | Bitwise ANDN (= AND . NOT) of two BigNum, resulting BigNum is positive.
andnBigNum :: BigNum -> BigNum -> BigNum
andnBigNum x@(BN# x#) y@(BN# y#)
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
  | isTrue# (eqBigNumWord# y 0##) = x
  | isTrue# (nx# >=# ny#) = andnBigNum' x# y# nx# ny#
  | True = andnBigNum' y# x# ny# nx#
 where
  nx# = wordsInBigNum# x
  ny# = wordsInBigNum# y

  -- assumes n# >= m# -- TODO(SN): test as gmp does something different?
  andnBigNum' a# b# n# m# = runS $ do
    mbn@(MBN# mba#) <- newBigNum n#
    mapWordArray# a# b# mba# (\a b -> a `and#` (not# b)) m#
    freezeBigNum mbn -- TODO(SN): resize mbn if possible

-- | Shift left logical, undefined for negative Int#.
shiftLBigNum :: BigNum -> Int# -> BigNum
shiftLBigNum x 0# = x
shiftLBigNum x _
  | isTrue# (eqBigNumWord# x 0##) = zeroBigNum
shiftLBigNum a@(BN# ba#) c# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  (I# i#) <- liftIO (bn_lshift mbr# nr# ba# na# c#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nr# = na# +# nwords# +# 1#
  nwords# = quotInt# c# WORD_SIZE_IN_BITS#

-- size_t integer_bn_lshift(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, size_t n) {
foreign import ccall unsafe "integer_bn_lshift"
  bn_lshift :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> IO Int

-- ** Arithmetic operations

-- | Add given Word# to BigNum.
plusBigNumWord :: Int# -- ^ Sign of number, 1# if negative
               -> BigNum -> Word# -> BigNum
plusBigNumWord neg# a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum na# -- TODO(SN): make sure enough allocated
  copyBigNum a r
  (I# i#) <- liftIO (bn_add_word neg# mbr# na# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a

-- size_t integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_add_word"
  bn_add_word :: Int# -> MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Subtract given Word# from BigNum.
minusBigNumWord :: Int# -- ^ Sign of number, 1# if negative
                -> BigNum -> Word# -> BigNum
minusBigNumWord neg# a w# = runS $ do
  r@(MBN# mbr#) <- newBigNum na# -- TODO(SN): make sure enough allocated
  copyBigNum a r
  (I# i#) <- liftIO (bn_sub_word neg# mbr# na# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
   na# = wordsInBigNum# a

-- size_t integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_sub_word"
  bn_sub_word :: Int# -> MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Multiply given BigNum with given Word#.
timesBigNumWord :: BigNum -> Word# -> BigNum
timesBigNumWord a@(BN# ba#) w# = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  copyBigNum a r
  (I# i#) <- liftIO (bn_mul_word mbr# nr# w#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nr# = na# +# 1#

-- int integer_bn_mul_word(BN_ULONG *rb, size_t rsize, BN_ULONG w)
foreign import ccall unsafe "integer_bn_mul_word"
  bn_mul_word :: MutableByteArray# s -> Int# -> Word# -> IO Int

-- | Multiply two BigNums.
timesBigNum :: BigNum -> BigNum -> BigNum
timesBigNum a@(BN# a#) b@(BN# b#) = runS $ do
  r@(MBN# mbr#) <- newBigNum nr#
  (I# i#) <- liftIO (bn_mul mbr# nr# a# na# b# nb#)
  shrinkBigNum r i# >>= freezeBigNum
 where
  na# = wordsInBigNum# a
  nb# = wordsInBigNum# b

  -- OpenSSL's BN_mul requires result BigNum to be big enough (bn_mul.c:553ff)
  nr# = case isTrue# (na# >=# nb#) of
    True  -> calculateSize na#
    False -> calculateSize nb#

  calculateSize n# =
    let j = lowerPowerTwo# n#
        k = j +# j
    in case isTrue# (k ># n#) of
      True -> k *# 4#
      False -> k *# 2#

  lowerPowerTwo# i# = 1# `uncheckedIShiftL#` (numBitsWord# (int2Word# i#) -# 1#)

  numBitsWord# w# = WORD_SIZE_IN_BITS# -# (word2Int# (clz# w#))

-- int integer_bn_mul(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, BN_ULONG *bb, size_t bsize)
foreign import ccall unsafe "integer_bn_mul"
  bn_mul :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> IO Int

-- | Divide a BigNum by a Word#, returning the quotient and a remainder (rounded
-- to zero). The divisor must not be 0##.
quotRemBigNumWord :: BigNum -> Word# -> (# BigNum, Word# #)
quotRemBigNumWord a 0## = (# a, remWord# 0## 0## #) -- raises division by zero
quotRemBigNumWord a 1## = (# a, 0## #)
quotRemBigNumWord a@(BN# ba#) w# = case runS divWord of (q, (W# r#)) -> (# q, r# #)
 where
  na# = wordsInBigNum# a
  nr# = na# +# 1#

  divWord s = do
    case newBigNum nr# s of { (# s1, r@(MBN# mbr#) #) ->
    case copyBigNum a r s1 of { (# s2, () #) ->
    case newByteArray# WORD_SIZE_IN_BYTES# s2 of { (# s3, mbrem# #) ->
    case liftIO (bn_div_word mbr# nr# w# mbrem#) s3 of { (# s4, (I# i#) #) ->
    case readWordArray# mbrem# 0# s4 of { (# s5, rem# #) ->
    case shrinkBigNum r i# s5 of { (# s6, r' #) ->
    case freezeBigNum r' s6 of { (# s7, r'' #) ->
    (# s7, (r'', (W# rem#)) #)
    }}}}}}}

-- int integer_bn_div_word(BN_ULONG *rb, size_t rsize, BN_ULONG w, BN_ULONG *rem)
foreign import ccall unsafe "integer_bn_div_word"
  bn_div_word :: MutableByteArray# s -> Int# -> Word# -> MutableByteArray# s -> IO Int

-- | Divide a BigNum by another BigNum, returning the quotient and a remainder
-- (rounded to zero). The divisor must not be 0##.
quotRemBigNum :: BigNum -> BigNum -> (# BigNum, BigNum #)
quotRemBigNum n d = (# n, n #) -- TODO WIP

-- ** Low-level BigNum creation and manipulation

-- | Create a MutableBigNum with given count of words.
newBigNum :: Int# -> S s (MutableBigNum s)
newBigNum count# s =
  -- Calculate byte size using shifts, e.g. for 64bit systems:
  -- total bytes = word count * 8 = word count * 2 ^ 3 = word count << 3
  case newByteArray# (count# `uncheckedIShiftL#` WORD_SHIFT#) s of
    (# s', mba# #) -> (# s', MBN# mba# #)

-- | Freeze a MutableBigNum into a BigNum.
freezeBigNum :: MutableBigNum s -> S s BigNum
freezeBigNum (MBN# mba#) s =
  case unsafeFreezeByteArray# mba# s of
    (# s', ba# #) -> (# s', BN# ba# #)

-- | Shrink a MutableBigNum the the given count of Word#.
shrinkBigNum :: MutableBigNum s -> Int# -> S s (MutableBigNum s)
shrinkBigNum mbn 0# s = (# s, mbn #)
shrinkBigNum mbn@(MBN# mba#) n# s
  | isTrue# (actual# ==# desired#) = (# s', mbn #)
  | True = case shrinkMutableByteArray# mba# desired# s' of s'' -> (# s'', mbn #)
 where
  !(# s', actual# #) = getSizeofMutableByteArray# mba# s
  desired# = n# `uncheckedIShiftL#` WORD_SHIFT#

-- | Write a word to a MutableBigNum at given word index. Size is not checked!
writeBigNum :: MutableBigNum s -> Int# -> Word# -> S s ()
writeBigNum (MBN# mba#) i# w# s =
  let s' = writeWordArray# mba# i# w# s

  in  (# s', () #)

-- | Copy magnitude from given BigNum into MutableBigNum.
copyBigNum :: BigNum -> MutableBigNum s -> S s ()
copyBigNum a@(BN# ba#) (MBN# mbb#) =
  copyWordArray# ba# 0# mbb# 0# (wordsInBigNum# a)

-- * ByteArray# utilities

-- | Copy multiples of Word# between ByteArray#s with offsets in words.
copyWordArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> S s ()
copyWordArray# src srcOffset dst dstOffset len s =
  let s' = copyByteArray# src srcOffsetBytes dst dstOffsetBytes lenBytes s

  in  (# s', () #)
 where
  srcOffsetBytes = srcOffset `uncheckedIShiftL#` WORD_SHIFT#
  dstOffsetBytes = dstOffset `uncheckedIShiftL#` WORD_SHIFT#
  lenBytes = len `uncheckedIShiftL#` WORD_SHIFT#

-- | Map over two ByteArray# for given number of words and store result in
-- MutableByteArray#.
mapWordArray# :: ByteArray# -> ByteArray# -> MutableByteArray# s
              -> (Word# -> Word# -> Word#)
              -> Int# -- ^ Number of words
              -> S s ()
mapWordArray# _ _ _ _ -1# s = (# s, () #)
mapWordArray# a# b# mba# f i# s =
  let w# = f (indexWordArray# a# i#) (indexWordArray# b# i#)
  in  case writeWordArray# mba# i# w# s of
        s' -> mapWordArray# a# b# mba# f (i# -# 1#) s'

-- * Internal functions

-- newBN :: IO BigNum
-- newBN = do
--   (W# w) <- bn_new
--   return $ BN# (unsafeCoerce# w)

-- -- TODO(SN): @IO Word@ as @IO Addr#@ and @(# State# RealWorld, Addr# #)@ not allowed
-- -- BIGNUM *BN_new(void)
-- foreign import ccall unsafe "BN_new" bn_new :: IO Word

-- freeBN :: BigNum -> IO ()
-- freeBN (BN# addr) = bn_free addr

-- -- void BN_free(BIGNUM *a);
-- foreign import ccall unsafe "BN_free" bn_free :: Addr# -> IO ()

-- newCtx :: IO BigNumCtx
-- newCtx = do
--   (W# w) <- bn_ctx_new
--   return $ CTX# (unsafeCoerce# w)

-- -- BN_CTX *BN_CTX_new(void);
-- foreign import ccall unsafe "BN_CTX_new" bn_ctx_new :: IO Word

-- freeCtx :: BigNumCtx -> IO ()
-- freeCtx (CTX# addr) = bn_ctx_free addr

-- -- void BN_CTX_free(BN_CTX *c);
-- foreign import ccall unsafe "BN_CTX_free" bn_ctx_free :: Addr# -> IO ()

-- bn2dec :: BigNum -> [Char]
-- bn2dec (BN# addr) = unpackCString# (bn_bn2dec addr)

-- -- char *BN_bn2dec(const BIGNUM *a);
-- foreign import ccall unsafe "BN_bn2dec" bn_bn2dec :: Addr# -> Addr#

-- bn2hex :: BigNum -> [Char]
-- bn2hex (BN# addr) = unpackCString# (bn_bn2hex addr)

-- -- char *BN_bn2hex(const BIGNUM *a);
-- foreign import ccall unsafe "BN_bn2hex" bn_bn2hex :: Addr# -> Addr#

-- setWord :: BigNum -> Word# -> IO ()
-- setWord (BN# addr) w = do
--   x <- bn_set_word addr w
--   case x of
--     1 -> return ()
--     _ -> IO $ fail "BN_set_word failed"

-- -- int BN_set_word(BIGNUM *a, BN_ULONG w);
-- foreign import ccall unsafe "BN_set_word" bn_set_word :: Addr# -> Word# -> IO Int

-- lshift :: BigNum -> Int# -> IO ()
-- lshift (BN# a) n
--   | isTrue# (n ==# 0#) = return ()
--   | isTrue# (n ># 0#) = do
--       x <- bn_lshift a a n
--       case x of
--         1 -> return ()
--         _ -> IO $ fail "BN_lshift failed"
--   | isTrue# (n <# 0#) = IO $ fail "BN_lshift negative n"

-- -- int BN_lshift(BIGNUM *r, const BIGNUM *a, int n);
-- foreign import ccall unsafe "BN_lshift" bn_lshift :: Addr# -> Addr# -> Int# -> IO Int

-- addBN :: BigNum -> BigNum -> IO BigNum
-- addBN (BN# a) (BN# b) = do
--   (BN# r) <- newBN
--   x <- bn_add r a b
--   case x of
--     1 -> return $ BN# r
--     _ -> runS $ fail "BN_add failed"

-- -- int BN_add(BIGNUM *r, const BIGNUM *a, const BIGNUM *b);
-- foreign import ccall unsafe "BN_add" bn_add :: Addr# -> Addr# -> Addr# -> IO Int

-- mulBN :: BigNum -> BigNum -> IO BigNum
-- mulBN (BN# a) (BN# b) = do
--   ctx@(CTX# c) <- newCtx
--   (BN# r) <- newBN
--   x <- bn_mul r a b c
--   freeCtx ctx
--   case x of
--     1 -> return $ BN# r
--     _ -> runS $ fail "BN_mul failed"

-- -- int BN_mul(BIGNUM *r, const BIGNUM *a, const BIGNUM *b, BN_CTX *ctx);
-- foreign import ccall unsafe "BN_mul" bn_mul :: Addr# -> Addr# -> Addr# -> Addr# -> IO Int

-- Foreign:

-- type role Ptr phantom
-- data Ptr a = Ptr Addr#
-- TODO(SN): add a managed Ptr to free on garbage collect (ForeignPtr)


-- * Borrowed things

-- ** From integer-gmp (requires -XRebindableSyntax):
-- monadic combinators for low-level state threading

type S s a = State# s -> (# State# s, a #)

infixl 1 >>=
infixl 1 >>
infixr 0 $

{-# INLINE ($) #-}
($) :: (a -> b) -> a -> b
f $ x = f x

{-# INLINE (>>=) #-}
(>>=) :: S s a -> (a -> S s b) -> S s b
(>>=) m k = \s -> case m s of (# s', a #) -> k a s'

{-# INLINE (>>) #-}
(>>) :: S s a -> S s b -> S s b
(>>) m k = \s -> case m s of (# s', _ #) -> k s'

{-# INLINE return #-}
return :: a -> S s a
return a = \s -> (# s, a #)

{-# INLINE liftIO #-}
liftIO :: IO a -> S RealWorld a
liftIO (IO m) = m

-- NB: equivalent of GHC.IO.unsafeDupablePerformIO, see notes there
runS :: S RealWorld a -> a
runS m = case runRW# m of (# _, a #) -> a

-- stupid hack
fail :: [Char] -> S s a
fail s = return (raise# s)

-- ** From Base

{-# INLINE (.) #-}
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- ** From GHC.Err:

undefined :: forall a. a
undefined = runS $ fail "Prelude.undefined"
