test_that("date to numeric: check time at risk", {
  library(dplyr)
  library(lubridate)


  data("tvtdata2")

  set.seed(42)

  data_num <-
    tvt_date2numeric(tvtdata2,
                     date_statrisk = stos,
                     date_treatna = treatna,
                     date_eos=eos,
                     state_eos=eos01,
                     date_eoatrisk = eotreat)

  test01<-
    tvtdata2%>%
    mutate(timeatrisk_fun =   data_num$time_atrisk)%>%
    mutate(timeatrisk_manu = (pmin(treatna,eos,eotreat,na.rm=T)-stos)/ddays(1),
           test_equal = timeatrisk_fun==timeatrisk_manu)%>%
    pull(test_equal)



  testthat::expect_true(all(test01), "Time at risk does not match")


})
