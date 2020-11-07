
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9001-orange.svg?style=flat-square)](commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--11--07-yellowgreen.svg)](/commits/master)

<!-- badges: end -->

## Disclaimer

To other soil scientists: Please do not label me a heretic simply from
the package name. I too appreciate the complexity and beauty of natural
soil bodies, and I am fully aware of connotations associated with those
who use the “d-word”. In this sense, the package name is a bit
tongue-in-cheek; I wrote the code for my PhD project on baseball infield
soils, and in baseball, like it or not, the infield skin is called
“dirt.”

## Utility

This package contains concise functions to analyze laboratory soil test
data. I wrote this package for my own use but other soil scientists may
find it of value. The code encompasses several commonly performed tests
(including particle size analysis, Proctor maximum density, and
Atterberg limits) and specialized analyses that were developed
specifically for this project. Examples include 3D morphometric analyses
of soil surfaces and numerical integration of stress-strain curves
derived from unconfined compression tests.

## Installation

An official version of `diRtscience` is not yet available on CRAN.
However, like other packages on GitHub, you can install it with
`devtools` or `remotes`:

``` r
remotes::install_github("evanmascitti/diRtscience")
```

<!-- The best way to learn about this package is through the vignettes  -->
<!-- un-comment this line once the vignettes are added  -->

## Functions

Below are terse descriptions of each function’s purpose:

`mix_calcs()`: Precise calculations for sand-clay soil mixtures

`compaction_aliquots()`: Specimen preparation for Proctor compaction
tests

`proctor_fit()`: Fit a compaction curve and compute the optimum water
content and maximum density.

`d_max()` Fit a compaction curve and compute the maximum density only.

`w_opt()` Fit a compaction curve and compute the optimum water content
only.

`add_w()`: Calculate gravimetric water content and add to an existing
data frame

## Standard method references

The standardized soil tests which may be deployed using this package
include:

#### Particle size analysis

-   Hydrometer method: [ASTM
    D7928-17](https://www.astm.org/Standards/D7928.htm)
-   Pipette method: [SSSA Methods of Soil Analysis, Part
    4](https://www.wiley.com/en-us/Methods+of+Soil+Analysis%2C+Part+4%3A+Physical+Methods-p-9780891188933)

#### Proctor compaction

-   Standard effort: [ASTM
    D698-12e2](https://www.astm.org/Standards/D698.htm)
-   Modified effort: [ASTM
    D1557-12e1](https://www.astm.org/Standards/D1557)

#### Atterberg limits

-   Canonical thread-rolling/Casagrande cup: [ASTM
    D4318-17e1](https://www.astm.org/Standards/D4318)
