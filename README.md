
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TVTmatching

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/jaromilfrossard/TVTmatching/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jaromilfrossard/TVTmatching?branch=master)
<!-- badges: end -->

The `TVTmatching` is an implement of a matching algorithm with
time-varying treatment. Each case is assign to one or several controls
such that their time “at risk of the treatment” matches.

## Installation

You can install the development version of `TVTmatching` like so:

``` r
remotes::intall_github("jaromilfrossard/TVTmatching")
```

## Example

You can explore the control candidates for each case using:

``` r
library(TVTmatching)
data("tvtdata")
tvt_sum <- tvt_summary_cases(tvtdata,
                             date_statrisk=stos,
                             date_treatna=treatna,
                             date_eos=eos,
                             state_eos=eos01, by = fct)
tvt_sum
#> # A tibble: 4 × 10
#>   id_row time_a…¹ time_…² state…³ state…⁴ fct   data_c…⁵ n_con…⁶ state…⁷ state…⁸
#>    <int>    <dbl>   <dbl>   <int>   <int> <chr> <list>     <int>   <int>   <int>
#> 1      1      366     731       1       1 A     <tibble>       2       1       1
#> 2      2      273     579       1       1 B     <tibble>       2       1       1
#> 3      3      122     821       1       0 A     <tibble>       3       1       2
#> 4      4       62     396       1       0 B     <tibble>       3       1       2
#> # … with abbreviated variable names ¹​time_atrisk, ²​time_eos, ³​state_treat,
#> #   ⁴​state_eos, ⁵​data_control, ⁶​n_control, ⁷​state_eos_0_control,
#> #   ⁸​state_eos_1_control
```

You can match using dates:

``` r
set.seed(42)
tvt_match<- tvt_matching_date(tvtdata,stos,treatna,eos,eos01)
tvt_match
#> # A tibble: 8 × 5
#>   group id_row treat_tv  time_eos_tv state_eos_tv
#>   <int>  <int> <chr>           <dbl>        <int>
#> 1     1      1 treatment         365            1
#> 2     1      5 control           396            0
#> 3     2      2 treatment         306            1
#> 4     2      8 control           154            0
#> 5     3      3 treatment         699            0
#> 6     3      1 control           244            0
#> 7     4      4 treatment         334            0
#> 8     4      1 control           304            0
```

If you have data in time you can use:

``` r
set.seed(42)
tvtdata_num <- tvt_date2numeric(tvtdata,
                                date_statrisk = stos,
                                date_treatna = treatna,
                                date_eos = eos,
                                state_eos = eos01)
tvt_match2 <- tvt_matching_num(tvtdata_num,
                               time_atrisk = time_atrisk,
                               time_eos = time_eos,
                               state_eos = state_eos,
                               state_treat = state_treat)
tvt_match2
#> # A tibble: 8 × 5
#>   group id_row treat_tv  time_eos_tv state_eos_tv
#>   <int>  <int> <chr>           <dbl>        <int>
#> 1     1      1 treatment         365            1
#> 2     1      5 control           396            0
#> 3     2      2 treatment         306            1
#> 4     2      8 control           154            0
#> 5     3      3 treatment         699            0
#> 6     3      1 control           244            0
#> 7     4      4 treatment         334            0
#> 8     4      1 control           304            0
```
