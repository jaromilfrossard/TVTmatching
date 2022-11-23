#' Pool estimates using the Rubin Rules
#'
#' @param estimate List. Each element of the list is a vector of pseudo estimates of length \eqn{m}.
#' @param std.error List. The standard error of the estimates.
#' @param df Numeric. Indicates the degrees of freedom of the test.
#' @param term Character. Indicates the names of the estimates.
#'
#' @details This function is adapted from the \code{mice::pool()} function.
#'
#' @return A data frame with the pooled results.
#'
#' @importFrom dplyr transmute group_by summarise case_when n
#' @importFrom purrr pmap
#' @importFrom stats pt var
#'
#' @references Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York: John Wiley and Sons.
#' @references Van Buuren, S., Groothuis-Oudshoorn, K., & Robitzsch, A. (2019). Package ‘mice’: multivariate imputation by chained equations. CRAN Repos.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' data(tvtdata2)
#'
#' list_data_surv <-
#' replicate(n = 3,
#'     {tvt_matching_date(tvtdata2,date_statrisk = stos,treatna,eos,eos01)},
#'     simplify = F)
#'
#' list_data_surv <- lapply(list_data_surv, \(di){
#'     di%>%
#'       bind_cols(tvtdata2%>%
#'       select(eos01,fct_2lvl,fct_4lvl,xcov)%>%
#'       slice(tvt_new$id_row))})
#'
#' list_coxph <- lapply(list_data_surv, \(di){
#'     coxph(Surv(time_eos_tv,state_eos_tv)~treat_tv+xcov,data= di)})
#'
#' estim <- lapply(list_coxph,coef)
#' var <- lapply(list_coxph,\(x){sqrt(diag(x$var))})
#' df <- list_coxph[[1]]$n-length(lestim[[1]])
#' term <- names(lestim[[1]])
#' pooling_rubin(estim,var,df,term)
#' }
#' @export
#' @family pooling
pooling_rubin <- function(estimate,std.error,df = NA_real_, term = NULL){

  if(is.null(term)){
    term <- as.character(seq_along(estimate[[1]]))
  }



  tibble(
    mi = seq_along(estimate),
    estimate = estimate,
    std.error = std.error)%>%
    transmute(mi= .data$mi,
              temp = pmap(list(.data$estimate,.data$std.error),
                          function(ei,stdi){
                            tibble(
                              term = term,
                              estimate = ei,
                              std.error = stdi)}))%>%
    unnest(.data$temp)%>%
    group_by(term)%>%
    summarise(
      m = n(),
      qbar = mean(.data$estimate),
      ubar = mean(.data$std.error^2),
      b = var(.data$estimate)
    )%>%
    mutate(t = .data$ubar + (1 + 1/.data$m) * .data$b,
           dfcom = case_when(length(.data$df)==n()~.data$df,
                             TRUE~rep(.data$df,n())),
           df = barnard.rubin(.data$m,.data$b,.data$t,.data$dfcom),
           riv = (1 + 1/.data$m) * .data$b/.data$ubar,
           lambda = (1 + 1/.data$m) * .data$b/.data$t,
           fmi = (.data$riv + 2/(.data$df + 3))/(.data$riv + 1))%>%
    mutate(std.error = sqrt(.data$t),
           statistic = .data$qbar/.data$std.error)%>%
    mutate(p.value = pt(abs(.data$statistic),.data$df,lower.tail = F)*2)

}


## COPY paste from mice:::barnard.rubin V 3.14.0
barnard.rubin <- function (m, b, t, dfcom = Inf){
  lambda <- (1 + 1/m) * b/t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1)/lambda^2
  dfobs <- (dfcom + 1)/(dfcom + 3) * dfcom * (1 - lambda)
  ifelse(is.infinite(dfcom), dfold, dfold * dfobs/(dfold +
                                                     dfobs))
}
