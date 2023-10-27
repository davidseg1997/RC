options(scipen = 999) # disable scientific notation

calc_ranks <- function(x) {
  ranks_table <- dplyr::select(x, geoid, asbest, total_rate, index_of_disparity, disparity_z, performance_z)

  #performance_rank: rank of 1 = best performance
  #if max is best then rank DESC / if min is best, then rank ASC. exclude NULLS.
  if(min(ranks_table$asbest) == 'max'){
    ranks_table$performance_rank = rank(desc(ranks_table$total_rate), na.last = "keep", ties.method = "min")
  } else
    if(min(ranks_table$asbest) == 'min'){
      ranks_table$performance_rank = rank(ranks_table$total_rate, na.last = "keep", ties.method = "min")
    }

  #disparity_rank: rank of 1 = worst disparity
  #max is worst, rank DESC. exclude NULLS.
  ranks_table$disparity_rank = rank(desc(ranks_table$index_of_disparity), na.last = "keep", ties.method = "min")

  #quadrants
  #if perf_z below avg and disp_z above avg, then red / perf_z above or avg and disp_z above or avg, then orange /
  #perf_z above or avg and disp_z below avg, then purple / perf_z below avg and disp_z below or avg, then yellow
  ranks_table$quadrant =
    ifelse(ranks_table$performance_z < 0 & ranks_table$disparity_z > 0, 'red',
           ifelse(ranks_table$performance_z > 0 & ranks_table$disparity_z >= 0, 'orange',
                  ifelse(ranks_table$performance_z >= 0 & ranks_table$disparity_z < 0, 'purple',
                         ifelse(ranks_table$performance_z < 0 & ranks_table$disparity_z <= 0, 'yellow', NA))))
  ### UPDATED QUADRANT CALCS: TO BE IMPLEMENTED IN V6 2024. THESE CHANGES ARE NOT YET IN EFFECT ON WEBSITE EITHER. ###
  # ifelse(ranks_table$performance_z < 0 & ranks_table$disparity_z > 0, 'red',
  # ifelse(ranks_table$performance_z >= 0 & ranks_table$disparity_z > 0, 'orange',
  # ifelse(ranks_table$performance_z >= 0 & ranks_table$disparity_z <= 0, 'purple',
  # ifelse(ranks_table$performance_z < 0 & ranks_table$disparity_z <= 0, 'yellow', NA))))

  ranks_table <- ranks_table %>% dplyr::select(geoid, disparity_rank, performance_rank, quadrant)
  x <- x %>% left_join(ranks_table , by = "geoid")

  return(x)
}
