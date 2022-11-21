test_that("Matching algorithm: censoring controls", {
  library(dplyr)


  data("tvtdata2")

  set.seed(42)

  tvt_new <-
    tvt_matching_date(tvtdata2,date_statrisk = stos,treatna,eos,eos01)

  test01 <- tvt_new%>%
    bind_cols(tvtdata2%>%
                select(eos01)%>%
                slice(tvt_new$id_row))%>%
    filter(treat_tv=="treatment")%>%
    mutate(test = state_eos_tv==eos01)%>%
    pull(test)

    testthat::expect_true(all(test01), "The treated do not have the same EOS state after matching.")


})
