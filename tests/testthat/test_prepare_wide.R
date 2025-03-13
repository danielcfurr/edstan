devtools::load_all()


# Wide data set up -------------------------------------------------------------

n_items <- 5
n_persons <- 10

wide <- as.data.frame(
  matrix(0:1, nrow = n_persons, ncol = n_items)
)

x <- 1:n_persons
x_std <- (x - mean(x)) / sd(x) / 2
b_std <- rep(c(-.5, .5), length.out = n_persons)

wide$constant <- 1
wide$continuous_1 <- x_std + 1
wide$continuous_2 <- x_std * 2
wide$binary_1 <- b_std + 1
wide$binary_2 <- b_std * 2


# Wide data that should provoke warnings ---------------------------------------

test_that("wide data with extra constant", {
  expect_warning(
    irt_data(response_matrix =  wide[, 1:n_items],
             covariates = wide[, -(1:n_items)],
             formula = ~ constant)
  )
})

test_that("wide data with missing intercept", {
  expect_warning(
    irt_data(response_matrix =  wide[, 1:n_items],
             covariates = wide[, -(1:n_items)],
             formula = ~ 0 + continuous_1)
  )
})

test_that("wide data with uncentered continuous covariate", {
  expect_warning(
    irt_data(response_matrix =  wide[, 1:n_items],
             covariates = wide[, -(1:n_items)],
             formula = ~ continuous_1)
  )
})

test_that("wide data with unscaled continuous covariate", {
  expect_warning(
    irt_data(response_matrix =  wide[, 1:n_items],
             covariates = wide[, -(1:n_items)],
             formula = ~ continuous_2)
  )
})

test_that("wide data with uncentered binary covariate", {
  expect_warning(
    irt_data(response_matrix =  wide[, 1:n_items],
             covariates = wide[, -(1:n_items)],
             formula = ~ binary_1)
  )
})

test_that("wide data with unscaled binary covariate", {
  expect_warning(
    irt_data(response_matrix =  wide[, 1:n_items],
             covariates = wide[, -(1:n_items)],
             formula = ~ binary_2)
  )
})


# Wide data that should be good ------------------------------------------------

test_that("wide data scaled no covariates", {
  expect_no_warning(
    capture.output(
      irt_data(response_matrix = wide[, 1:n_items])
    )
  )
})

wide_std <- data.frame(
  continuous_1 = x_std,
  continuous_2 = x_std,
  binary_1 = b_std,
  binary_2 = b_std
)

test_that("wide data scaled covariates without formula", {
  expect_no_warning(
      irt_data(response_matrix = wide[, 1:n_items],
               covariates = wide_std)
  )
})

test_that("wide data scaled covariates", {
  expect_no_warning(
    irt_data(response_matrix = wide[, 1:n_items],
             covariates = wide,
             formula = ~ rescale_continuous(continuous_1) +
               rescale_continuous(continuous_2) +
               rescale_binary(binary_1) +
               rescale_binary(binary_2))
  )
})

test_that("wide data scaled covariates with interaction", {
  expect_no_warning(
    irt_data(response_matrix = wide[, 1:n_items],
             covariates = wide,
             formula = ~ rescale_continuous(continuous_1) *
               rescale_continuous(continuous_2))
  )
})

test_that("wide data scaled covariates with square", {
  expect_no_warning(
    irt_data(response_matrix = wide[, 1:n_items],
             covariates = wide,
             formula = ~ rescale_continuous(continuous_1) +
               rescale_continuous(continuous_1)^2)
  )
})
