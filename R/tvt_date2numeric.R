#' @rdname tvt
#' @importFrom dplyr select pull bind_cols
#' @importFrom lubridate ddays
#' @importFrom tibble tibble
#' @export
tvt_date2numeric <- function(data,
                             date_statrisk,
                             date_treatna,
                             date_eos,
                             state_eos,
                             date_eoatrisk=NULL){

  mc <- match.call()

  if(is.null(mc$date_eoatrisk)){
    date_eoatrisk=mc$date_eos
    mc$date_eoatrisk=mc$date_eos
  }




  data_cov <- select(data,-c({{date_statrisk}},{{date_treatna}},{{date_eos}},
                             {{date_eoatrisk}},{{state_eos}}))

  # extract data
  date_statrisk <- pull(data,{{date_statrisk}})
  date_treatna <- pull(data,{{date_treatna}})
  date_eos <- pull(data,{{date_eos}})
  date_eoatrisk <- pull(data,{{date_eoatrisk}})
  state_eos <- pull(data,{{state_eos}})



  tibble(id_row = seq_len(nrow(data)),
         time_atrisk = (pmin(date_treatna,date_eos,date_eoatrisk,na.rm = T)-date_statrisk)/ddays(1),
         time_eos = (date_eos-date_statrisk)/ddays(1),
         state_treat = as.integer(!is.na(date_treatna)),
         state_eos = state_eos)%>%
    bind_cols(data_cov)

}
