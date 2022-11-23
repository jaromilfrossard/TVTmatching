#' Matching controls to cases with time-varying treatment
#'
#' @param data A data frame.
#' @param date_statrisk Column names indicating the date when patients start to be at risk of the treatment.
#' @param date_treatna Column names indicating the date of the start of the treatment and NA's for untreated subjects.
#' @param date_eos Column names indicating the date of the end of the study.
#' @param state_eos Column names indicating the state at the end of the study.
#' @param date_eoatrisk Column names indicating the date when patient end to be "at risk of the treatment". Specify \code{date_eos} the ending of the "at risk of the treatment" period corresponds to the end of the study.
#' @param ratio Integer. The number of controls per cases.
#' @param by Column names indicating a grouping factor for the matching.
#' @param time_atrisk Column names with integer representing the length of the "at risk of the treatment" period.
#' @param time_eos Column names with integer representing the length of the study.
#' @param state_treat Column names indicating if the subject experimented the treatment.
#' @param state_eos Column names indicating the state at the end of the study.
#'
#' @return
#' The \code{tvt_summary_cases()} function return a tibble with information of control candidates for each case.
#' The \code{tvt_date2numeric()} convert the encoding of the event from dates to numeric.
#' The \code{tvt_matching_date()} and \code{tvt_matching_num()} perform the matching algorithm.
#' @export
#'
#' @name tvt
#' @family tvt
#' @examples
#'
#' data("tvtdata")
#' tvtdata_num <- tvt_date2numeric(tvtdata,stos,treatna,eos,eos01)
#' tvt_sum <- tvt_summary_cases(tvtdata,stos,treatna,eos,eos01, by = fct)
#'
#' set.seed(42)
#' tvt_match<- tvt_matching_date(tvtdata,stos,treatna,eos,eos01)
#'
#'set.seed(42)
#'tvt_match2 <- tvt_matching_num(tvtdata_num,time_atrisk,time_eos,state_eos,state_treat)


#' @rdname tvt
#'
#' @export
tvt_matching_date <- function(data,
                        date_statrisk,
                        date_treatna,
                        date_eos,
                        state_eos,
                        date_eoatrisk = NULL,
                        ratio = 1,
                        by = NULL){


  mc <- match.call()

  if(is.null(mc$date_eoatrisk)){
    date_eoatrisk=mc$date_eos
    mc$date_eoatrisk=mc$date_eos
  }

  # extract data
  data_num <- tvt_date2numeric(
    select(data,c({{date_statrisk}},{{date_treatna}},{{date_eos}},{{state_eos}},{{date_eoatrisk}},{{by}})),
    date_statrisk={{date_statrisk}},date_treatna = {{date_treatna}},
    date_eos = {{date_eos}},state_eos = {{state_eos}},
    date_eoatrisk = {{date_eoatrisk}})

  tvt_matching_num(data_num,time_atrisk = quote(time_atrisk),time_eos = quote(time_eos),
                   state_treat=quote(state_treat),state_eos=quote(state_eos),ratio=ratio,
                   by = by)

}

#'@rdname tvt
#'@export
#'@importFrom dplyr bind_rows all_of
tvt_matching_num <- function(data, time_atrisk,time_eos,state_treat,state_eos,
                             ratio = 1,
                             by = NULL){

  data_cov <- select(data,-c({{time_atrisk}},{{time_eos}},{{state_treat}},
                             {{state_eos}}))
  time_atrisk <- pull(data,{{time_atrisk}})
  time_eos <- pull(data,{{time_eos}})
  state_treat <- pull(data,{{state_treat}})
  state_eos <- pull(data,{{state_eos}})
  #by <- pull(data,{{by}})


  #st2treat0_int <- date_statrisk%--%pmin(date_treatna,date_eos,date_eoatrisk,na.rm = T)
  #st2treat0_days <- time_length(st2treat0_int,"days")




  #ncase and treat
  ncase <- nrow(data)
  ntreateds <- sum(state_treat)


  # create grouping (weird because of lazy eval)
  grp <- data%>%
    dplyr::select(all_of({{by}}))
  if(ncol(grp)==0L){
    grp <-  rep(1L,ncase)
  }else{
    grp <-
      grp%>%
      interaction()
  }

  # warning ratio
  if(ntreateds*ratio>ncase){stop(paste0("Sample size (",ncase,") is too small for the number of treated (",ntreateds,") and the ratio (",ratio,")." ))}

  # prep loop

  ids_treated <- which(state_treat==1L)
  ids_controls <- seq_len(ncase)
  list_output <- list()


  for(groupi in seq_along(ids_treated)){

    # treated i
    id_treated_i <- ids_treated[groupi]
    time_i <- time_atrisk[id_treated_i]

    time_atrisk_centeri <- time_atrisk-time_i
    time_eos_centeri <- time_eos-time_i

    #remove case i
    select_ctr <- ids_controls!=id_treated_i

    #select time
    select_time <- time_atrisk_centeri>0

    #select grp
    select_grp <- grp==(grp[id_treated_i])

    ## pool selection
    select_pooled <-  select_ctr&select_time&select_grp

    #select control
    ids_contri <- ids_controls[select_pooled]


    #sample controls SAMPLE GUBS when length =1
    if(length(ids_contri)==1L){
      ids_contri <- rep(ids_contri,ratio)
    }else{
      ids_contri <- sample(ids_contri,ratio)
    }

    #create case tibble
    tb_case <- tibble(
      group = groupi,
      id_row = id_treated_i,
      treat_tv = "treatment",
      time_atrisk = time_atrisk[id_treated_i],
      time_eos_tv = time_eos_centeri[id_treated_i],
      state_eos_tv = state_eos[id_treated_i])


    ### censored treated at treatment date


    which_censored <- state_treat==1L
    state_eos_i <- state_eos
    state_eos_i[which_censored] <- 0L
    time_eos_centeri[which_censored]<-time_atrisk_centeri[which_censored]


    #create control tibble
    tb_control <- tibble(
      group = rep(groupi,ratio),
      id_row = ids_contri,
      treat_tv = rep("control",ratio),
      time_atrisk = time_atrisk[id_treated_i],
      time_eos_tv = time_eos_centeri[ids_contri],
      state_eos_tv = state_eos_i[ids_contri]
      )

    list_output[[groupi]] <- bind_rows(tb_case,tb_control)


  }

  do.call("rbind",list_output)


}




