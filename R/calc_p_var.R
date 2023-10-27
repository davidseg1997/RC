options(scipen = 999) # disable scientific notation

calc_p_var <- function(x) {
  suppressWarnings(rm(var))   #removes object 'var' if had been previously defined
  diffs <- dplyr::select(x, geoid, values_count, ends_with("_diff"))
  counts <- diffs %>% filter(values_count > 1) #%>% dplyr::select(-c(values_count))       #filter for counts >1
  counts$svar <- apply(counts[,3:ncol(counts)], 1, var, na.rm = T)                        #calc sample variance

  #convert sample variance to population variance. checked that the svar and variance results match VARS.S/VARS.P Excel results.
  #See more: https://stackoverflow.com/questions/37733239/population-variance-in-r
  counts$variance <- counts$svar * (counts$values_count - 1) / counts$values_count
  counts <- dplyr::select(counts, geoid, variance)                                        #remove extra cols before join
  x <- x %>% left_join(counts, by="geoid")                                    #join new variance column back to original table

  return(x)
}
