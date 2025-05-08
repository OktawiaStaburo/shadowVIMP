## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path   = "vignettes/figure-html/",
  fig.width  = 7,
  fig.height = 5
)

## ----eval = FALSE-------------------------------------------------------------
# devtools::install_github("OktawiaStaburo/shadowVIMP")

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(shadowVIMP)
library(magrittr)
library(dplyr)
library(ggplot2)

if (requireNamespace("AppliedPredictiveModeling", quietly = TRUE)) {
  library(AppliedPredictiveModeling)
} else {
  stop("Package 'AppliedPredictiveModeling' is required for this vignette. Please install it.")
}

