make_nn_data <- function(aug_trip_data, nb_sec = 3600) {
  nb_elements <- 86400 / nb_sec
  
  if(nrow(aug_trip_data) > 0) {
    res <- 
      aug_trip_data %>% 
      group_by(vin) %>% 
      summarise(
        prop = list(compute_proportion(time_start, time_end, nb_sec = nb_sec)), 
        claim_ind_cov_1_2_3_4_5_6 = first(claim_ind_cov_1_2_3_4_5_6)
      ) %>% 
      unnest(cols = c(prop)) %>% 
      mutate(names = rep(glue("x_{1:nb_elements}"), nrow(.) / nb_elements)) %>% 
      select(vin, prop, names, claim_ind_cov_1_2_3_4_5_6) %>% 
      pivot_wider(names_from = names, values_from = prop)
  } else {
    res <- tibble()
  }
  
  return(res)
}