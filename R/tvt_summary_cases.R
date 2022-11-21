#' @rdname tvt
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate filter across count inner_join rename_with starts_with
#' @importFrom purrr map map_int
#' @importFrom tidyr unnest pivot_wider replace_na
#' @importFrom rlang .data
#' @export
tvt_summary_cases <- function(data,
                        date_statrisk,
                        date_treatna,
                        date_eos,
                        state_eos,
                        date_eoatrisk = NULL,
                        by = NULL){

  mc <- match.call()

  if(is.null(mc$date_eoatrisk)){
    date_eoatrisk=mc$date_eos
    mc$date_eoatrisk=mc$date_eos
  }


  data_num <- tvt_date2numeric(
    select(data,c({{date_statrisk}},{{date_treatna}},{{date_eos}},{{state_eos}},{{date_eoatrisk}},{{by}})),
    date_statrisk={{date_statrisk}},date_treatna = {{date_treatna}},
    date_eos = {{date_eos}},state_eos = {{state_eos}},
    date_eoatrisk = {{date_eoatrisk}})



  by_full = c(as.character(select(data,{{by}})%>%colnames()))
  names(by_full)=by_full


  data_num%>%
    as_tibble()%>%
    filter(.data$state_treat==1L)%>%
    mutate(data_control = map(.data$id_row,\(ii){
      ti <- data_num%>%
        filter(.data$id_row==ii)%>%
        pull(.data$time_atrisk)

      data_num%>%
        filter(.data$id_row==ii)%>%
        select(c(quote(state_treat),{{by}}))%>%
        inner_join(filter(data_num,.data$time_atrisk>ti), by = by_full)
    }))%>%
    mutate(n_control = map_int(.data$data_control,nrow))%>%
    mutate(df = map(.data$data_control,\(di){
      di%>%
        count(.data$state_eos)%>%
        pivot_wider(names_from = .data$state_eos,values_from = .data$n,names_prefix = "state_eos_")%>%
        rename_with(\(x)paste0(x,"_control"))
    }))%>%
    unnest(.data$df,keep_empty = T)%>%
    mutate(across(starts_with("state_eos_"),replace_na,replace=0L))
}
