test_that("Summary of the cases: checking control per cases", {
  library(dplyr)


  data("tvtdata")

  set.seed(42)

  tvt_sum <- tvt_summary_cases(tvtdata,
                               date_statrisk = stos,
                               date_treatna=treatna,
                               date_eos=eos,
                               state_eos=eos01)

  test01 = tvt_sum$n_control ==c(4L,5L,6L,7L)

  testthat::expect_true(all(test01), "The numbers of controls for each case do not match the target.")


})
