
# geoxR

geoxR is a set of functions designed for modelling uplifts from ITV macro and micro region geo experiments.

Under the hood, geoxR uses meta's [GeoLift](https://facebookincubator.github.io/GeoLift/) project, combining it with accurate data about ITV's regional footprint and tools for connecting to Google Analytics web traffic as well as postcoded sales data.

## Installation

Install dependencies first.

``` r
# install.packages("remotes")
remotes::install_github("ebenmichael/augsynth")
remotes::install_github("facebookincubator/GeoLift")
remotes::install_github("neilc-itv/itvPalette")
remotes::install_github("itv/baRb")
```

You can install the development version of geoxR from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("neilc-itv/geoxR")
```

You need to setup BARB API and Google Auth credentials if you haven't already done this.

For BARB API credentials, see [baRb](https://github.com/ITV/baRb).

For Google Auth, create new credentials or use existing OAuth desktop credentials from [Google Cloud Console](https://console.cloud.google.com/apis/credentials).

Download the auth json and put it on your local machine *not* in your working directory, so that you won't accidentally push it to github. Then put the following line in your renviron: ```GAR_CLIENT_JSON=PATH_TO_YOUR_AUTH_FILE```

Problems with auth'ing with Google are beyond the scope of this readme. Please see [googleAuthR](https://cran.r-project.org/web/packages/googleAuthR/index.html)

## Example

geoxR reports are [Quarto](https://quarto.org/docs/computations/r.html) notebooks.

Create a new project using ```new_geoxR_project("foldername")``` and then open and edit 'template.qmd'.
