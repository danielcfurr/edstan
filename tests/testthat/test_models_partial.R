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


# Simplified PCM ---------------------------------------------------------------

test_that("fit simplified pcm", {
  expect_warning(
    irt_stan(dat,
             model = "pcm_simple.stan",
             iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})


# PCM --------------------------------------------------------------------------

test_that("fit pcm", {
  expect_warning(
    pcm <<- irt_stan(dat,
                     model = "pcm_latent_reg.stan",
                     iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})

test_that("print pcm", {
  expect_no_error(
    capture.output(
      print_irt_stan(pcm, dat)
    )
  )
})


# GPCM -------------------------------------------------------------------------

test_that("fit gpcm", {
  expect_warning(
    gpcm <<- irt_stan(dat,
                      model = "gpcm_latent_reg.stan",
                      iter = 10, chains = 1, refresh = 0, save_dso = FALSE)
  )
})

test_that("print gpcm", {
  expect_no_error(
    capture.output(
      print_irt_stan(gpcm, dat)
    )
  )
})
