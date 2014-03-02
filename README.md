#####GHC 7.6.3 vs 7.8-RC performance regression

This program uses xml-conduit to parse nzb files, a file format commonly found on Usenet.

Running the program

    ./mini demo1.nzb +RTS -sstderr

compiled with GHC 7.6.3 yields:

<pre>
    <b>37,827,736 bytes allocated in the heap
       703,392 bytes copied during GC
       172,040 bytes maximum residency (2 sample(s))
        35,024 bytes maximum slop
             2 MB total memory in use (0 MB lost due to fragmentation)

                                  Tot time (elapsed)  Avg pause  Max pause
    Gen  0        71 colls,     0 par    0.00s    0.00s     0.0000s    0.0001s
    Gen  1         2 colls,     0 par    0.00s    0.00s     0.0003s    0.0005s</b>

    TASKS: 3 (1 bound, 2 peak workers (2 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.00s  (  0.00s elapsed)
    MUT     time    0.01s  (  0.01s elapsed)
    GC      time    0.00s  (  0.00s elapsed)
    EXIT    time    0.00s  (  0.00s elapsed)
    <b>Total   time    0.02s  (  0.02s elapsed)</b>

    Alloc rate    2,603,976,578 bytes per MUT second

    Productivity  84.1% of total user, 86.9% of total elapsed
</pre>


The same program, same libraries but compiled with GHC 7.8.0.20140228 gives:

<pre>
    <b>11,217,541,448 bytes allocated in the heap
        31,184,656 bytes copied during GC
         6,540,288 bytes maximum residency (178 sample(s))
           597,248 bytes maximum slop
                19 MB total memory in use (5 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0     20623 colls,     0 par    0.50s    0.41s     0.0000s    0.0008s
    Gen  1       178 colls,     0 par    0.04s    0.05s     0.0003s    0.0009s</b>

    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.00s  (  0.00s elapsed)
    MUT     time    3.41s  (  3.50s elapsed)
    GC      time    0.55s  (  0.45s elapsed)
    EXIT    time    0.00s  (  0.00s elapsed)
    <b>Total   time    3.96s  (  3.96s elapsed)</b>

    Alloc rate    3,293,236,224 bytes per MUT second

    Productivity  86.1% of total user, 86.1% of total elapsed
</pre>

While total memory usage always stays around 2 to 3 mb when compiled with GHC 7.6.3, even if using bigger
nzb files (try demo2.nzb), the 7.8-RC compiled program eats huge amounts of memory and becomes very slow.

GHC 7.8-RC used was *ghc-7.8.0.20140228-x86_64-unknown-linux-deb7.tar.xz* found here: [haskell.org/ghc/dist/7.8.1-rc2](http://www.haskell.org/ghc/dist/7.8.1-rc2)



