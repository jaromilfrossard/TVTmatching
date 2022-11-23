test_that("Matching algorithm: censoring controls", {
  library(dplyr)


  data("tvtdata2")

  set.seed(42)

  tvt_new <-
    tvt_matching_date(tvtdata2,
                      date_statrisk = stos,
                      date_treatna = treatna,
                      date_eos=eos,
                      state_eos=eos01,
                      date_eoatrisk = eotreat)

  test01 <- tvt_new%>%
    bind_cols(tvtdata2%>%
                select(eos01)%>%
                slice(tvt_new$id_row))%>%
    filter(treat_tv=="treatment")%>%
    mutate(test = state_eos_tv==eos01)%>%
    pull(test)

    testthat::expect_true(all(test01), "The treated do not have the same EOS state after matching.")


})


test_that("Matching algorithm: time of controls", {
  library(dplyr)
  library(lubridate)


  data("tvtdata2")

  set.seed(43)

  tvt_ctr <-
    tvt_matching_date(tvtdata2,
                      date_statrisk = stos,
                      date_treatna = treatna,
                      date_eos=eos,
                      state_eos=eos01,
                      date_eoatrisk = eotreat)


  test02 <- tvt_ctr%>%
    bind_cols(tvtdata2%>%
                select(stos,treatna,eos,eotreat)%>%
                slice(tvt_ctr$id_row))%>%
    mutate(time_study = (pmin(treatna,eos,na.rm=T)-stos)/ddays(1))%>%
    mutate(equal_time = time_study==(time_atrisk+time_eos_tv))%>%
    filter(treat_tv=="control")%>%
    pull(equal_time)

  testthat::expect_true(all(test02), "The control do not have a time duration which matches the dates for: time at risk + time eos_tv.")


})


