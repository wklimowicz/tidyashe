
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyashe

<!-- badges: start -->

[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](Redirect-URL)
[![R-CMD-check](https://github.com/wklimowicz/tidyashe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wklimowicz/tidyashe/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Note: this has only been tested on ASHE files as they exist in DfE, and
may not work for ASHE `sav` files held elsewhere.

Combines and adds mappings to ASHE dataset.

## Installation

You can install tidyashe with:

``` r
# install.packages("remotes")
remotes::install_github("wklimowicz/tidyashe")
```

## Setup

``` r
library(tidyashe)

# Convert all sav's to fst
ashe_convert(folder = "ashe_sav_data/",
             new_folder = "ashe_fst_data/")

# Compile all fst's to a single fst
ashe <- ashe_compile("ashe_fst_data/")
```

To read in limited number of columns use `select_columns`:

``` r
keep_columns <- c("year", "piden", "thrs", "age",
                  "ft", "sex", "gpay", "sic07",
                  "sic03", "occ00", "occ10")

ashe_convert(folder = "ashe_sav_data/",
             new_folder = "ashe_fst_data/",
             select_columns = keep_columns)

ashe <- ashe_compile("ashe_fst_data/")
```
