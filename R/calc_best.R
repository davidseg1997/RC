options(scipen = 999) # disable scientific notation


calc_best <- function(x) {
  options(scipen = 999) # disable scientific notation
  rates <- dplyr::select(x, asbest, ends_with("_rate"), -ends_with("_no_rate"), -total_rate)
  rates[rates==0] <- NA                                                         #sub-out zero rates for NA, so that min best calc works correctly
  if (min(rates$asbest) == 'max') {rates <- rates %>% rowwise() %>%
    dplyr::mutate(best = max(c_across(where(is.numeric)), na.rm = TRUE))
  } else
    if (min(rates$asbest) == 'min') {rates <- rates %>% rowwise() %>%
      dplyr::mutate(best = min(c_across(where(is.numeric)), na.rm = TRUE))
    }
  rates$best[rates$best == Inf | rates$best == -Inf] <- NA
  x$best <- rates$best

  return(x)
}
