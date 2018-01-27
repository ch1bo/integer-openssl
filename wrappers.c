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

int integer_bn_add_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  BIGNUM r;
  r.d = rb;
  r.top = rsize;
  r.dmax = rsize - 1;
  r.neg = rneg;
  r.flags = 0;

  int ret = BN_add_word(&r, w);

  // Make sure that BIGNUM was not expanded in C-land
  assert(r.d == rb);

  // Return sign instead of ret,  TODO(SN) error handling?
  assert(ret == 1);
  return r.neg;
}

int integer_bn_sub_word(int rneg, BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  BIGNUM r;
  r.d = rb;
  r.top = rsize;
  r.dmax = rsize - 1;
  r.neg = rneg;
  r.flags = 0;

  int ret = BN_sub_word(&r, w);

  // Make sure that BIGNUM was not expanded in C-land
  assert(r.d == rb);

  // Return sign instead of ret,  TODO(SN) error handling?
  assert(ret == 1);
  return r.neg;
}
