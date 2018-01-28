#include <assert.h>
#include <openssl/bn.h>

// These functions wrap high-level openssl functions to work with word arrays
// from Haskell. Also, they assert that the passed BIGNUM pointers were not
// relocated by "expand" functions as that memory is managed by the Haskell RTS.
// Furthermore, most functions return the number of actually used words on the
// modified word array.

// Define BIGNUM aka bignum_st as bn_lcl.h is internal
typedef struct bignum_st BIGNUM;

struct bignum_st
{
  BN_ULONG *d;    /* Pointer to an array of 'BN_BITS2' bit chunks. */
  int top;        /* Index of last used d +1. */
  /* The next are internal book keeping for bn_expand. */
  int dmax;       /* Size of the d array. */
  int neg;        /* one if the number is negative */
  int flags;
};

// Macros to shorten BIGNUM declaration
#define S_BIGNUM(_name, _b, _size, _neg) \
  BIGNUM _name; \
  _name.d = _b; \
  _name.dmax = _size; \
  _name.top = _size; \
  _name.neg = _neg; \
  _name.flags = 0;
#define U_BIGNUM(_name, _b, _size) S_BIGNUM(_name, _b, _size, 0)

size_t integer_bn_lshift(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, size_t n) {
  U_BIGNUM(r, rb, rsize)
  r.top = 0;
  U_BIGNUM(a, ab, asize)
  int ret = BN_lshift(&r, &a, n);
  assert(ret == 1);
  assert(r.d == rb);
  return r.top;
}

int integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  S_BIGNUM(r, rb, rsize, rneg)
  int ret = BN_add_word(&r, w);
  assert(ret == 1);
  assert(r.d == rb);
  return r.top;
}

int integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  S_BIGNUM(r, rb, rsize, rneg)
  int ret = BN_sub_word(&r, w);
  assert(ret == 1);
  assert(r.d == rb);
  return r.top;
}

// Unfortunately the low-level functions are not available
// BN_ULONG bn_mul_words(BN_ULONG *rp, const BN_ULONG *ap, int num, BN_ULONG w);
int integer_bn_mul_word(BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  U_BIGNUM(r, rb, rsize);
  int ret = BN_mul_word(&r, w);
  assert(ret == 1);
  assert(r.d == rb);
  return r.top;
}

int integer_bn_mul(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, BN_ULONG *bb, size_t bsize) {
  U_BIGNUM(r, rb, rsize);
  U_BIGNUM(a, ab, asize);
  U_BIGNUM(b, bb, bsize);
  BN_CTX* ctx = BN_CTX_new();
  assert(ctx != NULL);
  int ret = BN_mul(&r, &a, &b, ctx);
  BN_CTX_free(ctx);
  assert(ret == 1);
  assert(r.d == rb);
  return r.top;
}
