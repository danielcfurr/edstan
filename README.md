# Overview

The **edstan** package for **R** provides convenience functions and pre-programmed **Stan** models related to item response theory (IRT). Its purpose is to make fitting common IRT models using Stan easy. 

The following table lists the models packaged with **edstan**. Each of these may optionally included a latent regression of ability. The **Stan** code for these models is documented in a series of case studies, linked in the table.

| Model | **Stan** file |
|--------------------------------------------------------------------------------------------------|-------------------------|
| [Rasch](http://mc-stan.org/documentation/case-studies/rasch_latent_reg.html)                     | *rasch_latent_reg.stan* |
| [Partical credit](http://mc-stan.org/documentation/case-studies/pcm_latent_reg.html)             | *pcm_latent_reg.stan*   |
| [Rating scale](http://mc-stan.org/documentation/case-studies/2pl_latent_reg.html)                | *rsm_latent_reg.stan*   |
| [Two-parameter logistic](http://mc-stan.org/documentation/case-studies/rsm_latent_reg.html)        | *2pl_latent_reg.stan*   |
| [Generalized partial credit](http://mc-stan.org/documentation/case-studies/gpcm_latent_reg.html) | *gpcm_latent_reg.stan*  |
| [Generalized rating scale](http://mc-stan.org/documentation/case-studies/grsm_latent_reg.html)   | *grsm_latent_reg.stan*  |

The next table lists the functions packaged with **edstan**.

| Function             | Purpose                                |
|----------------------|----------------------------------------|
| `irt_data()`         | Prepares data for fitting              |
| `labelled_integer()` | Create vector of consecutive integers  |
| `irt_stan()`         | Wrapper for Running MCMC               |
| `print_irt_stan()`   | Show table of output                   |
| `rhat_columns()`     | Create plot of convergence statistics  |


# Installation

**edstan** relies on the **rstan** package, which should be installed first.
[See here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) for instructions on installing **rstan**.

**edstan** is available on CRAN and may be installed with
`install.packages("edstan")`. 
Alternatively, the development version may be installed directly from Github as follows.

```{r}
# Install edstan development version of edstan
install.packages("devtools")
devtools::install_github("danielcfurr/edstan")
```


# Example

The R code below is an example how prepare data, fit the Rasch model, and then view results. It uses an example dataset packaged with **edstan**.

```{r}
# Load packages
library(rstan)
library(edstan)

# Make the data list
data_dich <- irt_data(y = aggression$dich, 
                      ii = labelled_integer(aggression$description), 
                      jj = aggression$person)

# Fit the Rasch model
fit_rasch <- irt_stan(data_dich, model = "rasch_latent_reg.stan",
                      iter = 200, chains = 4)

# View convergence statistics
rhat_columns(fit_rasch)

# View summary of parameter posteriors					  
print_irt_stan(fit_rasch, data_dich)

# Add a latent regression to the previous model
data_lr <- irt_data(y = aggression$dich, 
                    ii = labelled_integer(aggression$description), 
                    jj = aggression$person,
                    covariates = aggression[, c("male", "anger")],
                    formula = ~ 1 + male*aggression)
fit_lr <- irt_stan(data_lr, model = "rasch_latent_reg.stan",
                   iter = 200, chains = 4)
```
