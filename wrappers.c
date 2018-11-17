#include <assert.h>
#include <openssl/bn.h>

// These functions wrap high-level openssl functions to work with word arrays
// from Haskell. Also, they assert that the passed BIGNUM pointers were not
// relocated by "expand" functions as that memory is managed by the Haskell RTS.
// Furthermore, most functions return the number of actually used words on the
// modified word array.

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

int integer_bn_mul_word(BN_ULONG *rb, size_t rsize, BN_ULONG w) {
  U_BIGNUM(r, rb, rsize);
  r.top = rsize - 1; // rsize is +1 of actual used (see timesBigNumWord)
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

int integer_bn_div_word(BN_ULONG *qb, size_t qsize, BN_ULONG w, int32_t *qtop) {
  U_BIGNUM(r, qb, qsize);
  r.top = qsize - 1; // qsize is +1 of actual used (see quotRemBigNumWord)
  // printf("BN_div_word %s %lu %p\n", BN_bn2dec(&r), w, qtop);
  BN_ULONG ret = BN_div_word(&r, w);
  assert(ret != -1);
  assert(r.d == qb);
  *qtop = r.top;
  return ret;
}

int integer_bn_div(BN_ULONG *qb, size_t qsize,
                   BN_ULONG *remb, size_t remsize,
                   BN_ULONG *ab, size_t asize,
                   BN_ULONG *db, size_t dsize,
                   int32_t* qtop, int32_t* remtop) {
  U_BIGNUM(q, qb, qsize);
  q.top = qsize - 1; // qsize is +1 of actual used (see quotRemBigNum)
  U_BIGNUM(rem, remb, remsize);
  U_BIGNUM(a, ab, asize);
  U_BIGNUM(d, db, dsize);
  BN_CTX* ctx = BN_CTX_new();
  assert(ctx != NULL);
  // printf("BN_div %s %s %p %p\n", BN_bn2dec(&a), BN_bn2dec(&d), qtop, remtop);
  int ret = BN_div(&q, &rem, &a, &d, ctx);
  BN_CTX_free(ctx);
  assert(ret == 1);
  assert(q.d == qb);
  assert(rem.d == remb);
  // printf("q: %s, r: %s\n", BN_bn2dec(&q), BN_bn2dec(&rem));
  // printf("qtop: %d, remtop: %d\n", q.top, rem.top);
  *qtop = q.top;
  *remtop = rem.top;
  return ret;
}
