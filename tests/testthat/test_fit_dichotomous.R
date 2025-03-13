devtools::load_all()


# Data -------------------------------------------------------------------------

test_that("spelling data with covariates and formula", {
  expect_no_error(
    capture.output({
      dat <<- irt_data(response_matrix = spelling[, 2:5],
                       covariates = spelling,
                       formula = ~ male)
    })
  )
})


# Simplified model -------------------------------------------------------------

test_that("fit simplified rasch", {
  expect_warning(
    irt_stan(dat,
             model = "rasch_simple.stan",
             iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})


# Rasch ------------------------------------------------------------------------

test_that("fit rasch", {
  expect_warning(
    rasch <<- irt_stan(dat,
                       model = "rasch_latent_reg.stan",
                       iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})

test_that("print rasch", {
  expect_no_error(
    capture.output(
      print_irt_stan(rasch, dat)
    )
  )
})


# 2PL --------------------------------------------------------------------------

test_that("fit 2pl", {
  expect_warning(
    twopl <<- irt_stan(dat,
                       model = "2pl_latent_reg.stan",
                       iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})

test_that("print 2pl", {
  expect_no_error(
    capture.output(
      print_irt_stan(twopl, dat)
    )
  )
})
