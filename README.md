
# EpiPets

<!-- badges: start -->
<!-- badges: end -->

EpiPets is a little passion project of mine that was born out of frustration. This is a small collection of beginner-friendly helper functions for repetitive epidemiologic analyses. I wrote these functions to eliminate repetition in my own research, so I hope they can help others do the same.

EpiPets is intentionally opinionated. I wrote it for workflows I found myself writing over and over, so I built the functions to optimize *my* research. Please pay attention to the small nuances in each function if you encounter roadblocks. 

## Installation

You can install the development version of epipets from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("c1emence/epipets")
```

## Who Lives Here?

Here are examples of the three current functions that can be used in epipets.

`cooper()` is for simply formatted single-variable counts and percentages.
`tabby()` is a nicely-formatted cross-tabulation of two categorical variables.
`loggy()` is a quick simple logistic regression for odds ratios, 95% confidence intervals, and p-values.

``` r
library(epipets)

cooper(mtcars, cyl)

tabby(mtcars, cyl, gear)

loggy(mtcars, am, vs)
```

