options(scipen = 999) # disable scientific notation

calc_s_var <- function(x) {
  suppressWarnings(rm(var))   #removes object 'var' if had been previously defined
  diffs <- dplyr::select(x, geoid, values_count, grep("_diff", colnames(x)))
  counts <- diffs %>% filter(values_count > 1) %>% dplyr::select(-c(values_count))       #filter for counts >1
  counts$variance <- apply(counts[,-1], 1, var, na.rm = T)                        #calc sample variance
  counts <- dplyr::select(counts, -grep("_diff", colnames(counts)))                      #remove _diff cols before join
  x <- x %>% left_join(counts, by="geoid")                                    #join new variance column back to original table

  return(x)
}
