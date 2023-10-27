options(scipen = 999) # disable scientific notation

calc_z <- function(x) {
  #####calculate county disparity z-scores ----
  ## Total/Overall disparity_z score ##
  id_table <- dplyr::select(x, geoid, index_of_disparity)
  avg_id = mean(id_table$index_of_disparity, na.rm = TRUE) #calc avg id and std dev of id
  sd_id = sd((id_table$index_of_disparity), na.rm = TRUE)
  #mutate(sd_id = sd(unlist(id_table$index_of_disparity)))                    #calc avg id and std dev of id with unlist()
  id_table$disparity_z <- (id_table$index_of_disparity - avg_id) / sd_id      #note the disp_z results are slightly different than pgadmin, must be due to slight methodology differences
  x$disparity_z = id_table$disparity_z                                   #add disparity_z to original table

  ## Raced disparity_z scores ##
  diff <- dplyr::select(x, geoid, avg, index_of_disparity, variance, ends_with("_diff"))          #get geoid, avg, variance, and raced diff columns
  diff <- diff[!is.na(diff$index_of_disparity),]                                           #exclude rows with 2+ raced values, min is best, and lowest rate is 0
  diff_long <- pivot_longer(diff, 5:ncol(diff), names_to="measure_diff", values_to="diff") %>%   #pivot wide table to long on geoid & variance cols
    mutate(dispz=(diff - avg) / sqrt(variance), na.rm = TRUE) %>%                                   #calc disparity z-scores
    mutate(measure_diff=sub("_diff", "_disparity_z", measure_diff))                                #create new column names for disparity z-scores
  diff_wide <- diff_long %>% dplyr::select(geoid, measure_diff, dispz) %>%      #pivot long table back to wide keeping only geoid and new columns
    pivot_wider(names_from=measure_diff, values_from=dispz)
  x <- x %>% left_join(diff_wide, by="geoid")                           #join new columns back to original table

  #####calculate county performance z-scores
  ## Total/Overall performance z_scores ## Note the perf_z results are slightly different than pgadmin, must be due to slight methodology differences
  tot_table <- dplyr::select(x, geoid, asbest, total_rate)
  avg_tot = mean(tot_table$total_rate, na.rm = TRUE)      #calc avg total_rate and std dev of total_rate
  sd_tot = sd(tot_table$total_rate, na.rm = TRUE)
  if (min(tot_table$asbest) == 'max') {
    tot_table$performance_z <- (tot_table$total_rate - avg_tot) / sd_tot          #calc perf_z scores if MAX is best
  } else
    if (min(tot_table$asbest) == 'min') {
      tot_table$performance_z <-  ((tot_table$total_rate - avg_tot) / sd_tot) *-1   #calc perf_z scores if MIN is best
    }
  x$performance_z = tot_table$performance_z    #add performance_z to original table

  ## Raced performance z_scores ##
  rates <- dplyr::select(x, geoid, asbest, ends_with("_rate"), -ends_with("_no_rate"), -ends_with("_moe_rate"), -total_rate)  #get geoid, avg, variance, and raced diff columns
  avg_rates <- colMeans(rates[,3:ncol(rates)], na.rm = TRUE)                                        #calc average rates for each raced rate
  a <- as.data.frame(avg_rates)                                                                     #convert to data frame
  a$measure_rate  <- c(names(avg_rates))                                                            #create join field
  sd_rates <- sapply(rates[,3:ncol(rates)], sd, na.rm = TRUE)                                       #calc std dev for each raced rate
  s <- as.data.frame(sd_rates)                                                                      #convert to data frame
  s$measure_rate  <- c(names(sd_rates))                                                             #create join field
  rates_long <- pivot_longer(rates, 3:ncol(rates), names_to="measure_rate", values_to="rate")           #pivot wide table to long on geoid & variance cols
  rates_long <- left_join(rates_long, a, by="measure_rate")                                             #join avg rates for each raced rate
  rates_long <- left_join(rates_long, s, by="measure_rate")                                             #join std dev for each raced rate

  if (min(rates$asbest) == 'max') {
    rates_long <- rates_long %>% mutate(perf=(rate - avg_rates) / sd_rates, na.rm = TRUE) %>%         #calc perf_z scores if MAX is best
      mutate(measure_perf=sub("_rate", "_performance_z", measure_rate))                   #create new column names for performance z-scores
  } else
    if (min(rates$asbest) == 'min') {
      rates_long <- rates_long %>% mutate(perf=((rate - avg_rates) / sd_rates) *-1, na.rm = TRUE) %>%   #calc perf_z scores if MIN is best
        mutate(measure_perf=sub("_rate", "_performance_z", measure_rate))                   #create new column names for performance z-scores
    }

  rates_wide <- rates_long %>% dplyr::select(geoid, measure_perf, perf) %>%          #pivot long table back to wide keeping only geoid and new columns
    pivot_wider(names_from=measure_perf, values_from=perf)

  x <- x %>% left_join(rates_wide, by = "geoid")                                #join new columns back to original table

  return(x)
}
