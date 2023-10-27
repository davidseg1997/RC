options(scipen = 999) # disable scientific notation

calc_avg_diff <- function(x) {
  diffs <- dplyr::select(x, geoid, values_count, ends_with("_diff"))
  counts <- diffs %>% filter(values_count > 1) %>% dplyr::select(-c(values_count))     #filter for counts >1
  counts$avg <- rowMeans(counts[,-1], na.rm = TRUE)                             #calc avg diff
  counts <- dplyr::select(counts, -grep("_diff", colnames(counts)))                      #remove _diff cols before join
  x <- x %>% left_join(counts, by="geoid")                                    #join new avg diff column back to original table

  return(x)
}
