options(scipen = 999) # disable scientific notation

calc_id <- function(x) {
  diffs <- dplyr::select(x, geoid, best, asbest, values_count, ends_with("_diff"))
  diffs$sumdiff <- ifelse(diffs$values_count==0, NA, rowSums(diffs[,-c(1:4)], na.rm=TRUE))    #calc sum of diff from best
  #ID calc returns NA when there are <2 raced values OR where there are 2 raced values, MIN is best, and the sum of diffs = best.
  #The second condition is where MIN is best, a geo has only 2 rates and one of them is 0.
  diffs$index_of_disparity <- ifelse(diffs$values_count < 2 | diffs$values_count == 2 & diffs$asbest == 'min' & diffs$sumdiff == diffs$best, NA, (((diffs$sumdiff / diffs$best) / (diffs$values_count - 1)) * 100))
  x$index_of_disparity <- diffs$index_of_disparity

  return(x)
}
