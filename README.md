
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diRtscience

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Note: I am still in the process of tweaking a few functions. The package should be fully updated by Nov 1. 2020 .

## Installation

An official version of `diRtscience` is not yet available on CRAN.
However, you can install it with the `devtools` package by using the
command:

``` r
devtools::install_github("evanmascitti/diRtscience")
```

This package contains concise functions to analyze laboratory soil test
data. I wrote the package primarily for my own use, but other soil scientists may also
find it of value. The code encompasses several commonly performed lab tests
(including particle size analysis, Proctor maximum density, and
Atterberg limits) and specialized analyses that were developed
specifically for this project. Examples include 3D morphometric analyses
of soil surfaces and numerical integration of stress-strain curves
derived from unconfined compression tests.

<!-- The best way to learn about this package is through the vignettes  -->

<!-- un-comment this line once the vignettes are added  -->

## Utility

The function `sandClay.mixCalcs` is supplied for generating the air-dry
masses of soil to use when batch-blending sand-clay mixtures. It is a
relatively simple solution to a 2-member system of equations which
accounts for the hygroscopic water content of each soil and its
respective % sand-size particles. The user may choose any desired final
% sand and so long as the water contents of each soil are known, the
final mixture will contain the desired sand % on an oven-dry mass basis.

The standardized soil tests which may be deployed using this package
include:

#### Particle size analysis

  - Hydrometer method: [ASTM
    D7928-17](https://www.astm.org/Standards/D7928.htm)
  - Pipette method: [SSSA Methods of Soil Analysis,
    Part 4](https://www.wiley.com/en-us/Methods+of+Soil+Analysis%2C+Part+4%3A+Physical+Methods-p-9780891188933)

#### Proctor compaction

  - Standard effort: [ASTM
    D698-12e2](https://www.astm.org/Standards/D698.htm)
  - Modified effort: [ASTM
    D1557-12e1](https://www.astm.org/Standards/D1557)

#### Atterberg limits

  - [ASTM D4318-17e1](https://www.astm.org/Standards/D4318)
