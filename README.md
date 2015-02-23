## How to set up package repository locally with Rstudio

1. Select from the menus __File__ >> __New project...__.
2. Select the __Version control__ option in the window that appears.
3. Select the __Git__ option.
4. Enter https://github.com/danielcfurr/edstan.git in the __Repository URL__ field. Also provide the folder you want to download the repository to. Click the __Create project__ button.
5. Provide your Github username and password when prompted.

These steps will create an Rstudio file named _edstan.Rproj_ in the _edstan_ directory. Opening this file opens the Rstudio project associated with _edstan_, which will allow you to build the _edstan_ package locally and push/pull updates with Github.


## Overview of package repository

### Folders

* __R__: Contains all the R code.
* __data__: Contains the example datasets. These are in a particular format created with the `devtools::use_data()` function.
* __inst/extdata__: Contains the Stan program files. The location of these files when installed as a package may be found using this R command: `system.file("extdata", "stancode_twopl.stan", package = "edstan")`.
* __man__: Contains the help files. These files are created based on "meta-comments" in the R code and should not be edited manually.

### Files

* DESCRIPTION
* NAMESPACE


## How to build the _edstan_ package

1. Select from the menus __Build__ >> __Document__. This is an optional step that updates the help files.
2. Select from the menus __Build__ >> __Build and reload__.

These step compile the _edstan_ package and install it to your computer. Then you can access it in R with `library(edstan)`.