# integer-openssl

Implementation of the `GHC.Integer` type using the OpenSSL BIGNUM arbitrary size
integer library contained in `libcrypto`.

## **WARNING** Work in progress

Currently this is a proof of concept and only some of the `Integer` functions
are implemented. If things work out, this would be a free BSD-licensed
alternative to the LGPL-licensed `integer-gmp` - used by default in GHC - with
performance significantly faster than `integer-simple` - a BSD-licensed
Haskell-only integer library.

The test suite gives a rough estimate in what is currently implemented as each
function is checked against the builtin library. First benchmarks indicate a
10-20% performance hit against `integer-gmp` but significant speedup compared to
`integer-simple`:

```
integer-openssl-0.1.0: benchmarks
Running 1 benchmarks...
Benchmark integer-openssl-bench: RUNNING...
benchmarking mkInteger/Library
time                 152.1 μs   (147.8 μs .. 157.0 μs)
                     0.994 R²   (0.991 R² .. 0.996 R²)
mean                 151.4 μs   (148.3 μs .. 156.2 μs)
std dev              11.85 μs   (8.588 μs .. 18.25 μs)
variance introduced by outliers: 72% (severely inflated)

benchmarking mkInteger/Builtin
time                 110.1 μs   (105.7 μs .. 115.0 μs)
                     0.985 R²   (0.977 R² .. 0.992 R²)
mean                 111.1 μs   (107.9 μs .. 115.2 μs)
std dev              12.58 μs   (9.995 μs .. 16.65 μs)
variance introduced by outliers: 85% (severely inflated)

benchmarking timesInteger/Small Library
time                 35.83 ms   (35.46 ms .. 36.21 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 36.00 ms   (35.81 ms .. 36.39 ms)
std dev              580.4 μs   (272.0 μs .. 973.6 μs)

benchmarking timesInteger/Small Builtin
time                 58.69 ms   (57.36 ms .. 60.16 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 59.28 ms   (58.64 ms .. 60.98 ms)
std dev              1.679 ms   (748.7 μs .. 2.706 ms)

benchmarking timesInteger/Big Library
time                 135.3 ms   (134.5 ms .. 135.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 135.7 ms   (135.3 ms .. 136.6 ms)
std dev              702.4 μs   (195.4 μs .. 1.019 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking timesInteger/Big Builtin
time                 4.424 s    (4.393 s .. 4.451 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.420 s    (4.414 s .. 4.423 s)
std dev              5.660 ms   (0.0 s .. 5.934 ms)
variance introduced by outliers: 19% (moderately inflated)

Benchmark integer-openssl-bench: FINISH
```

More recent benchmarks with small, 128bit and 4096bit integers of multiplication and division:

* [integer-openssl vs. integer-gmp](https://github.com/ch1bo/integer-openssl/blob/master/openssl-vs-gmp.html)
* [integer-openssl vs. integer-simple](https://github.com/ch1bo/integer-openssl/blob/master/openssl-vs-simple.html)

## License

The source code for `integer-openssl` is released under the [BSD-3-Clause License](https://opensource.org/licenses/BSD-3-Clause).
