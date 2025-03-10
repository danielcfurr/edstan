devtools::load_all()


# Data -------------------------------------------------------------------------

test_that("verbal aggression data with covariates and formula", {
  expect_no_error({
    dat <<- irt_data(y = aggression$poly,
                     ii = labelled_integer(aggression$description),
                     jj = aggression$person,
                     covariates = aggression,
                     formula = ~ 1 + male*anger)
  })
})


# Simplified model -------------------------------------------------------------

test_that("fit simple rsm", {
  expect_warning(
    irt_stan(dat,
             model = "rsm_simple.stan",
             iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})


# RSM --------------------------------------------------------------------------

test_that("fit rsm", {
  expect_warning(
    rsm <<- irt_stan(dat,
                     model = "rsm_latent_reg.stan",
                     iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})

test_that("print rsm", {
  expect_no_error(
    capture.output(
      print_irt_stan(rsm, dat)
    )
  )
})


# GRSM -------------------------------------------------------------------------

test_that("fit grsm", {
  expect_warning(
    grsm <<- irt_stan(dat,
                      model = "grsm_latent_reg.stan",
                      iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})

test_that("print grsm", {
  expect_no_error(
    capture.output(
      print_irt_stan(grsm, dat)
    )
  )
})


