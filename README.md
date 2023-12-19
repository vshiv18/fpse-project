# OCaml-BigBWT

(601.429/629) class project
by Nate Brown, Vikram Shivakumar, Jacopo Teneggi

---

## Purpose

The Burrows-Wheeler Transform (BWT) [1] is a commonly used pre-processing algorithm for lossless compression in computational genomics. Naive algorithms for BWT construction involve sorting all suffixed of a string, which can be computationally intractable for long tests (e.g., millions of charactes in a genome).

We aim to implement an efficient algorithm for BWT construction using the recently proposed prefix-free parsing (PFP) [2] method for suffix array construction.

With this, we will create a library for exact string matching over the BWT using the FM-index [3], which performs string matching in time proportional to the length of the query.

---

## Libraries

1. [Wavelet trees](https://github.com/dymil/wavelet-trees) to implement one of the underlying data structures for the indices. We were able to install this library and run its benchmarks.

We copied the source code from the above wavelet tree library to be built under our dune structure.

---

## Usage

Our command-line interface provides two commands:

* `bwt` to compute and save the BWT of an input file or load a pre-computed BWT from a directory:

```console
_build/default/src/bwt.exe --help
OCaml BigBWT

  bwt.exe 

More detailed information

=== flags ===

  [--from-parse string]      . Path to directory to load parse from.
  [--out-dir string]         . Path to store parse results.
  [--window int]             . Window size.
  [-i string]                . Path to file to compute BWT of.
  [-build-info]              . print info about this build and exit
  [-version]                 . print the version of this build and exit
  [-help], -?                . print this help text and exit```
```

For example, run:

```console
_build/default/src/bwt.exe -i <input_file>.fa --out-dir <output_dir>
```

to compute the BWT of the sequence stored in `<input_file>.fa` and save the results to `<output_dir>`. Subsequently, run:

```console
_build/default/src/bwt.exe --from-parse <output_dir>
```

to load the parse.

* `fm_indexer` to count the number of occurrences of a pattern in a string:

```console
_build/default/src/fm_indexer.exe --help 
OCaml FM index

  fm_indexer.exe MODE

More detailed information

=== flags ===

  [--out-prefix string]      . Path to store index.
  [-i string]                . Path to file to build index for.
  [-p string]                . Pattern to search for.
  [-build-info]              . print info about this build and exit
  [-version]                 . print the version of this build and exit
  [-help], -?                . print this help text and exit
```

which has two modes of execution:

1. `build` which builds the FM-Index of an input file and saves it. For example:

```console
_build/default/src/fm_indexer.exe build -i <input_file>.fa --out-prefix <output_prefix>
```

builds the FM-index of the sequence stored in `<input_file>.fa` and saves the results to `<output_prefix>`. Note that this function relies on the BWT computation function above.

2. `run` which loads a pre-computed FM-Index and counts the number of occurrences of pattern `-p`. For example:

```console
_build/default/src/fm_indexer.exe run -i <index> -p ACTG
```

counts the number of occurrences of `ACTG` in the index stored in `<index>`.

---

## Code References

We did not use any pre-existing code for our project.
We will implement existing algorithms from the literature:

1. [https://gitlab.com/manzai/Big-BWT](https://gitlab.com/manzai/Big-BWT)
2. [https://github.com/alshai/pfbwt-f](https://github.com/alshai/pfbwt-f)

---

## References

[1] [*Burrows-Wheeler transform* on Wikipedia](https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform)
[2] [Oliva, Marco, Travis Gagie, and Christina Boucher. "Recursive Prefix-Free Parsing for Building Big BWTs." 2023 Data Compression Conference (DCC). IEEE, 2023.](https://ieeexplore.ieee.org/abstract/document/10125303)
[3] [Ferragina, Paolo, and Giovanni Manzini. "Indexing compressed text." Journal of the ACM (JACM) 52.4 (2005): 552-581.](https://dl.acm.org/doi/abs/10.1145/1082036.1082039)
[4] [Gagie, Travis, Gonzalo Navarro, and Nicola Prezza. "Fully functional suffix trees and optimal text searching in BWT-runs bounded space." Journal of the ACM (JACM) 67.1 (2020): 1-54.](https://dl.acm.org/doi/abs/10.1145/3375890)
