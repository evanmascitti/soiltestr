
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

To other soil scientists: Please do not label me a heretic simply from
the package name. I too appreciate the complexity and beauty of natural
soil bodies. The package name is a bit tongue-in-cheek; I wrote this
code for my PhD project on baseball infield soils….and in baseball, like
it or not, the infield skin is called “dirt.”

## Installation

An official version of `diRtscience` is not yet available on CRAN.
However, if you have `devtools` or `remotes` installed, you can install
it with the command:

``` r
remotes::install_github("evanmascitti/diRtscience")
```

This package contains concise functions to analyze laboratory soil test
data. I wrote this package for my own use but other soil scientists may
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

## Methods included

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

-   [ASTM D4318-17e1](https://www.astm.org/Standards/D4318)
