devtools::load_all()

# Note: There are fewer tests for long format data because it should not be
# necessary to repeat the tests for validating latent regressions.


# Data -------------------------------------------------------------------------

n_items <- 5
n_persons <- 10
n_covariates <- 3
n_responses <- n_items * n_persons

ii <- rep(1:n_items, times = n_persons)
jj <- rep(1:n_persons, each = n_items)
y <- rep(0:2, length.out = n_responses)

covariates_base <- data.frame(
  matrix(rnorm(n_persons * n_covariates), ncol = n_covariates)
)

covariates <- covariates_base[jj, ]

covariates_differ_within_person <- covariates
covariates_differ_within_person$problematic <- rnorm(nrow(covariates))


# Tests ------------------------------------------------------------------------

test_that("long data no covariates", {
  expect_no_warning(
    irt_data(y = y,
             ii = ii,
             jj = jj)
  )
})

test_that("long data with covariates no formula", {
  expect_no_warning(
    irt_data(y = y,
             ii = ii,
             jj = jj,
             covariates = covariates,
             validate_regression = FALSE)
  )
})

test_that("long data with covariates and formula", {
  expect_no_warning(
    irt_data(y = y,
             ii = ii,
             jj = jj,
             covariates = covariates,
             formula = ~ .,
             validate_regression = FALSE)
  )
})

test_that("long data with covariates varying within person", {
  expect_error(
    irt_data(y = y,
             ii = ii,
             jj = jj,
             covariates = covariates_differ_within_person,
             validate_regression = FALSE)
  )
})



