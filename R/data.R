#' Small testing data
#'
#' A simulated dataset
#'
#' @format a dataframe with 8 rows and 5 columns:
#'
#' \describe{
#'    \item{stos}{Date. Starting date of the study.}
#'    \item{treatna}{Date. Date of the time-varying treatment. Contains NA's for untreated patients.}
#'    \item{eos}{Date. Date of the end of the study.}
#'    \item{eos01}{Integer. State at the end of the study.}
#'    \item{fct}{Character. A 2 level factor.}
#' }
#' @family data
"tvtdata"


#' Large testing data
#'
#' A simulated dataset
#'
#' @format a dataframe with 200 rows and 9 columns:
#'
#' \describe{
#'    \item{stos}{Date. Starting date of the study.}
#'    \item{treatna}{Date. Date of the time-varying treatment. Contains NA's for untreated patients.}
#'    \item{eos}{Date. Date of the end of the study.}
#'    \item{eostreat}{Date. Date of the end of the period at risk of treatment.}
#'    \item{eos01}{Integer. State at the end of the study.}
#'    \item{fct_2lvl}{Character. A 2 level factor.}
#'    \item{fct_4lvl}{Character. A 4 level factor.}
#'     \item{xcov}{Integer. A covariate of numeric.}
#' }
#' @family data
"tvtdata2"
