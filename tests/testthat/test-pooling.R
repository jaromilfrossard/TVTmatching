
test_that("Rubin pooling rules", {
  library(mice)
  library(dplyr)
  library(purrr)

  set.seed(42)
  imp <- mice(nhanes, maxit = 2, m = 3)
  fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))

  tb0 <-
    tibble(models = fit$analyses)%>%
    mutate(id = 1:n())%>%
    mutate(tidy = map(models,tidy))


  mice_pooled <- summary(pool(fit))

  tvt_pool <- pooling_rubin(
    estimate = lapply(tb0$tidy,\(x)x$estimate),
    std.error = lapply(tb0$tidy,\(x)x$std.error),
    df = fit$analyses[[1]]$df.residual,
    term = names(coef(fit$analyses[[1]])))



  test01 <- abs(mice_pooled$p.value-tvt_pool$p.value)<1e-10


  testthat::expect_true(all(test01), "Rubin rules does not match with mice package.")



  })
