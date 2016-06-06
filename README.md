# Overview

The edstan package for R provides a convenience functions and several pre-programmed Stan item response theory (IRT) models. It's purpose is to make fitting some IRT models using Stan easy. 

The following table lists the models packaged with edstan. Each of these may optionally included a latent regression of ability. The Stan code for these models is documented in a series of case studies, linked in the table.

| Model | Stan file |
|--------------------------------------------------------------------------------------------------|-------------------------|
| [Rasch](http://mc-stan.org/documentation/case-studies/rasch_latent_reg.html)                     | *rasch_latent_reg.stan* |
| [Partical credit](http://mc-stan.org/documentation/case-studies/pcm_latent_reg.html)             | *pcm_latent_reg.stan*   |
| [Rating scale](http://mc-stan.org/documentation/case-studies/2pl_latent_reg.html)                | *rsm_latent_reg.stan*   |
| [Two-parameter logistic](http://mc-stan.org/documentation/case-studies/rsm_latent_reg.html)        | *2pl_latent_reg.stan*   |
| [Generalized partial credit](http://mc-stan.org/documentation/case-studies/gpcm_latent_reg.html) | *gpcm_latent_reg.stan*  |
| [Generalized rating scale](http://mc-stan.org/documentation/case-studies/grsm_latent_reg.html)   | *grsm_latent_reg.stan*  |

The next table lists the functions packaged with edstan.

| Function             | Purpose                                |
|----------------------|----------------------------------------|
| `irt_data()`         | Prepares data for fitting              |
| `labelled_integer()` | Create vector of consecutive integers  |
| `irt_stan()`         | Wrapper for Running MCMC               |
| `print_irt_stan()`   | Show table of output                   |

# Installation

This package is still in development and is not yet available on CRAN. Instead, it may installed directly from Github with help from the devtools package. Below is R code for installing rstan, devtools, and then edstan.

```{r}
# Install rstan
install.packages("rstan")

# Install edstan (requires devtools)
install.packages("devtools")
devtools::install_github("danielcfurr/edstan")
```


# Example

The R code below is an example how prepare data, fit the Rasch model, and then view results. It uses an example dataset packaged with edstan.

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

# View summary of parameter posteriors					  
print_irt_stan(fit_rasch, data_dich)
```
