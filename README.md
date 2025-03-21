# edstan

The edstan package for **R** streamlines Bayesian estimation of item response
models. It is designed around several pre-defined [Stan](https://mc-stan.org/)
models and functions that support a quick start to a Bayesian workflow. 


## Installation

edstan relies on the rstan package, which should be installed first.
[See here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) for 
instructions on installing rstan.

Once rstan is working on your machine, edstan may then be installed from CRAN:

```r
install.packages("edstan")
```

Alternatively, edstan may be installed from Github:

```r
devtools::install_github("danielcfurr/edstan")
```

## Brief manual

A [brief manual](briefmanual.md) provides details on the functions and 
models. It also provides links to tutorials providing in depth information on
each of the available models.


## Quick start guide

### Wide-format data

The `spelling` dataset has one row per person. The first column `male` is a 
dummy variable for whether the respondent is male. The remaining four columns
are dummy variables for whether the respondent spelled a word correctly.

```r
library(edstan)

# Use parallel processing
options(mc.cores = parallel::detectCores())

# Prepare data
spelling_dat <- irt_data(response_matrix = spelling[, -1])

# Fit a model to the prepared data
spelling_fit <- irt_stan(spelling_dat)

# View sampling diagnostics
stan_columns_plot(spelling_fit)

# Display a summary of parameter posteriors
print_irt_stan(spelling_fit, spelling_data)
```

### Long-format data

The `aggression` dataset has one row per item response. The column `person` has
ID values indicating the person providing the response, `item` has
ID values for the item responded to, and `poly` has values 0, 1, or 2 for the
item response.

```r
library(edstan)

# Use parallel processing
options(mc.cores = parallel::detectCores())

# Prepare data
aggression_dat <- irt_data(ii = aggression$person,
                           jj = aggression$item,
                           y = aggression$poly)

# Fit a model to the prepared data
aggression_fit <- irt_stan(aggression_dat)

# View sampling diagnostics
stan_columns_plot(aggression_fit)

# Display a summary of parameter posteriors
print_irt_stan(aggression_fit, aggression_data)
```

