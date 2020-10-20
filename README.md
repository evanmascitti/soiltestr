
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

#### \*\* *Note: I am still in the process of tweaking a few functions. The package should be fully updated by Nov 1. 2020 .* \*\*

## Covering my bases with other soil scientists

Please don't be enraged with my use of the "d-word." I'm keenly aware that soils are complex three-dimensional bodies warranting our respect and our study. But on a baseball field, we call it dirt. Let's not dwell on pedantic minutiae and instead move on to something more interesting than terminology grudges. 

## Installation

An official version of `diRtscience` is not yet available on CRAN.
However, if you have `devtools` or `remotes` installed, you can install
it with the command:

``` r
remotes::install_github("evanmascitti/diRtscience")
```

This package contains concise functions to analyze laboratory soil test
data. I wrote this code for my own use but other soil scientists may
find it of value. The code encompasses several commonly performed tests
(including particle size analysis, Proctor maximum density, and
Atterberg limits) and specialized analyses that were developed
specifically for this project. Examples include 3D morphometric analyses
of soil surfaces and numerical integration of stress-strain curves
derived from unconfined compression tests.

<!-- The best way to learn about this package is through the vignettes  -->

<!-- un-comment this line once the vignettes are added  -->

## Utility

Brief descriptions of each function:

**mix\_calcs**: Precise calculations for sand-clay soil mixtures

**compaction\_aliquots**: Specimen preparation for Proctor compaction
tests

**proctor\_fit**: Fit a compaction curve and compute the optimum water
content and maximum density.

**add\_w**: Calculate gravimetric water content and add to an existing
data frame

## Methods included

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
