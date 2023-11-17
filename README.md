# OCaml-BigBWT

(601.429/629) class project
by Nate Brown, Vikram Shivakumar, Jacopo Teneggi

---

## Purpose

The Burrows-Wheeler Transform (BWT) [1] is a commonly used pre-processing algorithm for lossless compression in computational genomics. Naive algorithms for BWT construction involve sorting all suffixed of a string, which can be computationally intractable for long tests (e.g., millions of charactes in a genome).

We aim to implement an efficient algorithm for BWT construction using the recently proposed prefix-free parsing (PFP) [2] method for suffix array construction.

With this, we will create a library for exact string matching over the BWT using two indexing approaches:

1. **FM-index** [3]: performs string matching in time proportional to the length of the query.
2. **r-index** [4]: performs string matching in space proportional to the number of repeated character runs in the BWT.

---

## Libraries

1. [Biocaml](https://github.com/biocaml/biocaml) to parse FASTA files. We were able to install this library, and we included tests for its FASTA parsing functionalities in `tests/fasta.ml`.
2. [Wavelet trees](https://github.com/dymil/wavelet-trees) to implement one of the underlying data structures for the indices. We were able to install this library and run its benchmarks.

---

## Usage

Our command-line interface will have two commands:

* `build` to write the index to a file:

```console
~$ ./bigbwt.exe build seq.fasta --out index.output --mode [FM|r]
Reading sequence from seq.fasta...
Sequence: GATTACAT!GATACAT!GATTAGATA
Building [FM|r]-index...
Saved [FM|r]-index to index.output
```

* `query` to find the locations of a substring in the text:

```console
~$ ./bigbwt.exe query GAT --index index.output
Finding `GAT` in index.output...
Found `GAT` at positions 0, 9, 18, 23
```

---

## Implementation Plan

- [x] Implement a naive BWT algorithm.
- [x] Implement a PFP algorithm with underlying hashing library.
- [ ] Implement an efficient BWT algorithm using PFP.
- [ ] Implement FM-index.
- [ ] Implement r-index.
- [ ] Implement serialization functions for both indices.
- [ ] Implement command-line interface to build indices.
- [ ] Implement command-line interface to query indices.

Potential extensions:

- [ ] Parallelization
- [ ] Index/text query framework for genome alignment

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
