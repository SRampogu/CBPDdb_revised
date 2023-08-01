## ----style, echo = FALSE, results = 'asis'--------------------------------------------------------
BiocStyle::markdown()
options(width=100, max.print=1000)
knitr::opts_chunk$set(
    eval=as.logical(Sys.getenv("KNITR_EVAL", "TRUE")),
    cache=as.logical(Sys.getenv("KNITR_CACHE", "TRUE")))

## ----setup, echo=FALSE, messages=FALSE, warnings=FALSE--------------------------------------------
suppressPackageStartupMessages({
    library(ChemmineOB)
})

## ----eval=FALSE, tidy=FALSE-----------------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly=TRUE))
#      install.packages("BiocManager")
#  BiocManager::install(c("ChemmineR", "ChemmineOB"))
#  library("ChemmineR")
#  library("ChemmineOB")

## ----eval=FALSE, tidy=FALSE-----------------------------------------------------------------------
#   vignette("ChemmineR")

## ----sessionInfo, results='asis'------------------------------------------------------------------
sessionInfo()

