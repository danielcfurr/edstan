# Overview of package repository

## Folders

* __R__: Contains all the R code.
* __data__: Contains the example datasets. These are in a particular format created with the `devtools::use_data()` function.
* __inst/extdata__: Contains the Stan program files. The location of these files when installed as a package may be found using this R command: `system.file("extdata", "stancode_twopl.stan", package = "edstan")`.
* __man__: Contains the help files. These files are created based on "meta-comments" in the R code and should not be edited manually.

## Files

* DESCRIPTION
* NAMESPACE


# How to build the _edstan_ package

1. Select from the menus __Build__ >> __Document__. This is an optional step that updates the help files.
2. Select from the menus __Build__ >> __Build and reload__.

These step compile the _edstan_ package and install it to your computer. Then you can access it in R with `library(edstan)`.
