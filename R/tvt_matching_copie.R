# #'Matching with time-varying treatment
# #'
# #' @param data a data frame
# #' @param date_statrisk column names with the date when patients start to be at risk of the treatment.
# #' @param date_treatna column names with the date of the start of the treatment and NA's for untreated patients.
# #' @param date_eos column names with the date of the end of the study
# #' @param state_eos column names with the state at the end of the study
# #' @param date_eoatrisk column names with the date when patient end to be at risk of the treatment. Specify \code{date_eos}
# #' @param ratio
# #' @param remove_cases
# #' @param by
# #'
# #' @return
# #' @export
# #'
# #' @examples
# tvt_matching <- function(data,
#                               date_statrisk,
#                               date_treatna,
#                               date_eos,
#                               state_eos,
#                               date_eoatrisk,
#                               ratio = 1,
#                               remove_cases = F,
#                               by = NULL){
#
#
#   mc <- match.call()
#
#   # extract data
#   date_statrisk <- pull(data,{{date_statrisk}})
#   date_treatna <- pull(data,{{date_treatna}})
#   date_eos <- pull(data,{{date_eos}})
#   date_eoatrisk <- pull(data,{{date_eoatrisk}})
#   state_eos <- pull(data,{{state_eos}})
#
#
#
#   st2treat0_int <- date_statrisk%--%pmin(date_treatna,date_eos,date_eoatrisk,na.rm = T)
#   st2treat0_days <- time_length(st2treat0_int,"days")
#
#
#
#
#   #ncase and treat
#   ncase <- nrow(data)
#   ntreateds <- sum(!is.na(date_treatna))
#
#
#   #create grouping (weird because of lazy eval)
#   grp <- dplyr::select(data,{{by}})
#   if(ncol(grp)==0L){
#     grp <-  rep(1L,ncase)
#   }else{
#     grp <-
#       grp%>%
#       interaction()
#   }
#
#   # warning ratio
#   if(ntreateds*ratio>ncase){stop(paste0("Sample size (",ncase,") is too small for the number of treated (",ntreateds,") and the ratio (",ratio,")." ))}
#
#   # prep loop
#
#   ids_treated <- which(!is.na(date_treatna))
#   ids_controls <- seq_len(ncase)
#   list_output <- list()
#
#
#
#
#   for(groupi in seq_along(ids_treated)){
#
#     # treated i
#     id_treated_i <- ids_treated[groupi]
#     time_i <- st2treat0_days[id_treated_i]
#     treat2eos_comp2i <- st2treat0_days-time_i
#
#     #remove case i
#     select_ctr <- ids_controls!=id_treated_i
#
#     #select time
#     select_time <- treat2eos_comp2i>0
#
#     #select grp
#     select_grp <- grp==(grp[id_treated_i])
#
#     # remove case
#     if(remove_cases){
#       select_case <- is.na(date_treatna)
#     }else{
#       select_case <- rep(TRUE,length(date_treatna))
#
#     }
#
#
#     ## pool selection
#     select_pooled <-  select_ctr&select_time&select_grp&select_case
#     if(sum(select_pooled)<ratio){
#       select_pooled <-  select_ctr&select_time&select_grp&select_case
#
#     }
#
#
#     #select control
#     ids_contri <- ids_controls[select_pooled]
#
#
#     #sample controls SAMPLE GUBS when length =1
#     if(length(ids_contri)==1L){
#       ids_contri <- rep(ids_contri,ratio)
#     }else{
#       ids_contri <- sample(ids_contri,ratio)
#
#     }
#
#     ## contol time/state Treated controls are censored
#     which_treated_ctr <- !is.na(date_treatna[ids_contri])
#     ncc_date_eos_ctr <- date_eos[ids_contri]
#     ncc_date_eos_ctr[which_treated_ctr] <- (date_treatna[ids_contri])[which_treated_ctr]
#     ncc_eos_state_ctr <- state_eos[ids_contri]
#     ncc_eos_state_ctr[which_treated_ctr] <- 0L
#
#     #create case tibble
#     tb_case <- tibble(
#       group = groupi,
#       row_id = id_treated_i,
#       ncc_treat = "treatment",
#       ncc_start2treat_time = time_i,
#       ncc_treat_date = date_statrisk[id_treated_i]+ddays(time_i),
#       ncc_date_eos = date_eos[id_treated_i],
#       ncc_eos_state = state_eos[id_treated_i])
#
#     #create control tibble
#     tb_control <- tibble(
#       group = rep(groupi,ratio),
#       row_id = ids_contri,
#       ncc_treat = rep("control",ratio),
#       ncc_start2treat_time = time_i,
#       ncc_treat_date = date_statrisk[ids_contri]+ddays(time_i),
#       ncc_date_eos = ncc_date_eos_ctr,
#       ncc_eos_state = ncc_eos_state_ctr)
#
#     list_output[[groupi]] <- bind_rows(tb_case,tb_control)
#
#
#   }
#
#   do.call("rbind",list_output)
#
#
# }
#
#
#
#
#
#
#
#
# # nested case control
# ncc_old <- function(treat_time,treatment,data,ratio = 1, by = NULL){
#
#
#   mc <- match.call()
#
#   # extract data
#   treateds <- pull(data,{{treatment}})
#   times <- pull(data,{{treat_time}})
#
#   #ncase and treat
#   ncase <- nrow(data)
#   ntreateds <- sum(treateds)
#
#
#   #create grouping (weird because of lazy eval)
#   grp <- dplyr::select(data,{{by}})
#   if(ncol(grp)==0L){
#     grp <-  rep(1L,ncase)
#   }else{
#     grp <-
#       grp%>%
#       interaction()
#   }
#
#
#
#
#   # warning ratio
#   if(ntreateds*ratio>ncase){stop(paste0("Sample size (",ncase,") is too small for the number of treated (",ntreateds,") and the ratio (",ratio,")." ))}
#
#   # prep loop
#
#   ids_treated <- which(treateds==1)
#   ids_controls <- seq_len(ncase)
#   list_output <- list()
#
#   rand_treated <- sample(ids_treated)
#
#
#
#   for(groupi in seq_along(rand_treated)){
#     # treated i
#     id_treated_i <- rand_treated[groupi]
#     #remove event i (for all loop)
#     ids_controls <- ids_controls[ids_controls!=id_treated_i]
#
#     #select time (for loop i)
#     select_time <- times[ids_controls]>=times[id_treated_i]
#
#     #select grp (for loop i)
#     select_grp <- grp[ids_controls]==grp[id_treated_i]
#
#     ids_contri <- ids_controls[select_time& select_grp]
#     #remove groups (for loop i)
#
#     #sample in controls
#     ids_contri <- sample(ids_contri,ratio)
#
#     #create case tibble
#     tb_case <- tibble(
#       group = groupi,
#       row_id = id_treated_i,
#       ncc_treat = "treatment",
#       ncc_treat_time = times[id_treated_i])
#
#     #create control tibble
#     tb_control <- tibble(
#       group = rep(groupi,ratio),
#       row_id = ids_contri,
#       ncc_treat = rep("control",ratio),
#       ncc_treat_time = times[id_treated_i])
#
#     list_output[[groupi]] <- bind_rows(tb_case,tb_control)
#
#
#   }
#
#   do.call("rbind",list_output)
#
#
# }
#
#









