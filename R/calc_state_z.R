options(scipen = 999) # disable scientific notation


calc_state_z <- function(x) {
  ## Raced disparity z-scores ##
  diff <- dplyr::select(x, geoid, avg, index_of_disparity, variance, ends_with("_diff"))          #get geoid, avg, variance, and raced diff columns
  diff <- diff[!is.na(diff$index_of_disparity),]                                           #exclude rows with 2+ raced values, min is best, and lowest rate is 0
  diff_long <- pivot_longer(diff, 5:ncol(diff), names_to="measure_rate", values_to="rate") %>%   #pivot wide table to long on geoid & variance cols
    mutate(diff=(rate - avg) / sqrt(variance)) %>%                                               #calc disparity z-scores
    mutate(measure_diff=sub("_diff", "_disparity_z", measure_rate))                              #create new column names for disparity z-scores
  diff_wide <- diff_long %>% dplyr::select(geoid, measure_diff, diff) %>%      #pivot long table back to wide keeping only geoid and new columns
    pivot_wider(names_from=measure_diff, values_from=diff)
  x <- x %>% left_join(diff_wide, by="geoid")                           #join new columns back to original table

  return(x)
}
