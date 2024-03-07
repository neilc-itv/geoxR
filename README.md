
# geoxR

geoxR is a set of functions designed for modelling uplifts from ITV macro and micro region geo experiments.

Under the hood, geoxR uses meta's [GeoLift](https://facebookincubator.github.io/GeoLift/) project, combining it with accurate data about ITV's regional footprint and tools for connecting to Google Analytics web traffic as well as postcoded sales data.

## Installation

You can install the development version of geoxR from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("neilc-itv/geoxR")
```

## Example

geoxR reports are [Quarto](https://quarto.org/docs/computations/r.html) notebooks.

Create a new project using ```new_geoxR_project("foldername")``` and then open and edit 'template.qmd'.
