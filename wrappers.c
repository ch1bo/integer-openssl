#include <assert.h>
#include <openssl/bn.h>

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

size_t integer_bn_lshift(BN_ULONG *rb, size_t rsize, BN_ULONG *ab, size_t asize, size_t n) {
  BIGNUM r;
  r.d = rb;
  r.top = 0;
  r.dmax = rsize;
  r.neg = 0;
  r.flags = 0;

  BIGNUM a;
  a.d = ab;
  a.top = asize;
  a.dmax = asize;
  a.neg = 0;
  a.flags = 0;

  int ret = BN_lshift(&r, &a, n);
  // TODO(SN) error handling?
  assert(ret == 1);
  // Make sure that BIGNUM was not expanded in C-land
  assert(r.d == rb);
  return r.top; // Return utilized len
}

int integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  BIGNUM r;
  r.d = rb;
  r.top = rsize;
  r.dmax = rsize;
  r.neg = rneg;
  r.flags = 0;

  int ret = BN_add_word(&r, w);
  // TODO(SN) error handling?
  assert(ret == 1);
  // Make sure that BIGNUM was not expanded in C-land
  assert(r.d == rb);
  return r.top; // Return utilized len
}

int integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  BIGNUM r;
  r.d = rb;
  r.top = rsize;
  r.dmax = rsize;
  r.neg = rneg;
  r.flags = 0;

  int ret = BN_sub_word(&r, w);
  // TODO(SN) error handling?
  assert(ret == 1);
  // Make sure that BIGNUM was not expanded in C-land
  assert(r.d == rb);
  return r.top; // Return utilized len
}
