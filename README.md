Minimalistic library for encoding JSON directly to strict bytestring.

The library focuses on 2 aspects: simplicity and performance.
The API consists of just a few functions and
achieves performance that is 3 times better than that of "aeson"
in typical use-cases.
No case in which "aeson" is performing better has been observed.

Following are the benchmark results comparing the performance
of encoding typical documents using this library and "aeson".
The numbers after the slash identify the amount of objects in
the rendered JSON.
"lazy-aeson" stands for "aeson" producing a lazy bytestring.

```
jsonifier/1          mean 1.970 μs  ( +- 21.85 ns  )
jsonifier/10         mean 12.12 μs  ( +- 259.9 ns  )
jsonifier/100        mean 117.4 μs  ( +- 2.291 μs  )
jsonifier/1000       mean 1.295 ms  ( +- 22.47 μs  )
jsonifier/10000      mean 20.48 ms  ( +- 1.076 ms  )
aeson/1              mean 6.687 μs  ( +- 106.5 ns  )
aeson/10             mean 31.19 μs  ( +- 845.4 ns  )
aeson/100            mean 262.8 μs  ( +- 6.484 μs  )
aeson/1000           mean 2.944 ms  ( +- 207.0 μs  )
aeson/10000          mean 29.94 ms  ( +- 957.9 μs  )
lazy-aeson/1         mean 6.454 μs  ( +- 114.4 ns  )
lazy-aeson/10        mean 30.69 μs  ( +- 773.1 ns  )
lazy-aeson/100       mean 259.2 μs  ( +- 2.485 μs  )
lazy-aeson/1000      mean 2.542 ms  ( +- 60.51 μs  )
lazy-aeson/10000     mean 25.42 ms  ( +- 269.7 μs  )
```

The benchmark suite is bundled with the package, so you can observe
the results yourself.

For the example of usage check out the "demo".
It comes bundled as well.

The quality of the library is ensured with a test property in which
a random "aeson" value is generated, then rendered using "jsonifier",
then parsed with "aeson" and compared to the original.
