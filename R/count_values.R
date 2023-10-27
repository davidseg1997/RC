options(scipen = 999) # disable scientific notation

#' Count the number of rate values
#'
#' @param D
#'
#' @return A column with values_count
#' @export
#'
#' @examples
#' d <- count_values(d)

count_values <- function(x) {
  rates <- dplyr::select(x, ends_with("_rate"), -ends_with("_no_rate"), -total_rate)
  rates$values_count <- rowSums(!is.na(rates))
  x$values_count <- rates$values_count

  return(x)
}
