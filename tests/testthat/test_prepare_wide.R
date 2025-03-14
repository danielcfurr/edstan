# Response matrices ------------------------------------------------------------

test_that("wide data basic response matrix", {
  expect_no_warning(
    irt_data(response_matrix =  matrix(c(0, 1, 2), nrow=10, ncol=3))
  )
})

test_that("wide data response matrix with NA", {
  expect_warning(
    irt_data(response_matrix =  matrix(c(0, NA, 2), nrow=10, ncol=3))
  )
})

test_that("wide data bad matrix without zero category", {
  expect_warning(
    irt_data(response_matrix =  matrix(c(1, 2), nrow=10, ncol=3))
  )
})

test_that("wide data response matrix without middle category", {
  expect_warning(
    irt_data(response_matrix =  matrix(c(0, 2), nrow=10, ncol=3))
  )
})


# Data for latent regressions --------------------------------------------------

n_items <- 5
n_persons <- 10

responses <- as.data.frame(
  matrix(0:1, nrow = n_persons, ncol = n_items)
)

x <- 1:n_persons
x_std <- (x - mean(x)) / sd(x) / 2
b_std <- rep(c(-.5, .5), length.out = n_persons)

scaled_covariates <- data.frame(
  continuous_1 = x_std,
  continuous_2 = x_std,
  binary_1 = b_std,
  binary_2 = b_std
)

unscaled_covariates <- data.frame(
  constant = 1,
  continuous_1 = x_std + 1,
  continuous_2 = x_std * 2,
  binary_1 = b_std + 1,
  binary_2 = b_std * 2
)


# Regressions that should be good ------------------------------------------------

test_that("wide data scaled covariates without formula", {
  expect_no_warning(
    irt_data(response_matrix = responses,
             covariates = scaled_covariates)
  )
})

test_that("wide data scaled covariates", {
  expect_no_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ rescale_continuous(continuous_1) +
               rescale_continuous(continuous_2) +
               rescale_binary(binary_1) +
               rescale_binary(binary_2))
  )
})

test_that("wide data scaled covariates with interaction", {
  expect_no_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ rescale_continuous(continuous_1) *
               rescale_continuous(continuous_2))
  )
})

test_that("wide data scaled covariates with square", {
  expect_no_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ rescale_continuous(continuous_1) +
               rescale_continuous(continuous_1)^2)
  )
})


# Regressions that should provoke warnings -------------------------------------

test_that("wide data with extra constant", {
  expect_warning(
    irt_data(response_matrix =  responses,
             covariates = unscaled_covariates,
             formula = ~ constant)
  )
})

test_that("wide data with missing intercept", {
  expect_warning(
    irt_data(response_matrix = responses,
             covariates = scaled_covariates,
             formula = ~ 0 + continuous_1)
  )
})

test_that("wide data with uncentered continuous covariate", {
  expect_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ continuous_1)
  )
})

test_that("wide data with unscaled continuous covariate", {
  expect_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ continuous_2)
  )
})

test_that("wide data with uncentered binary covariate", {
  expect_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ binary_1)
  )
})

test_that("wide data with unscaled binary covariate", {
  expect_warning(
    irt_data(response_matrix = responses,
             covariates = unscaled_covariates,
             formula = ~ binary_2)
  )
})
