
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diRtscience

<!-- badges: start -->

<!-- badges: end -->

## Installation

This package is not yet available on CRAN. However, you can install it
with the `devtools` package by using the command:

``` r
devtools::install_github("evanmascitti/diRtscience")
```

This package contains consise functions to analyze laboratory soil test
data. The code was developed by Evan Mascitti while earning his PhD at
Penn State. The package encompasses several commonly performed tests
(including particle size analysis, Proctor maximum density, and
Atterberg limits) and specialized analyses that were developed
specifically for this project. Examples include morphometric analyses of
3D soil surfaces and numerical integration of stress-strain curves
derived from unconfined compression tests.

<!-- The best way to learn about this package is through the vignettes  -->

<!-- un-comment this line once the vignettes are added  -->

The function `sandClay.mixCalcs` is supplied for generating the air-dry
masses of soil which should be used when batch-blending sand-clay
mixtures.

The soil which may be deployed using this package include:

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
