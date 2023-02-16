keep_one_contract <- function(aug_trip_data) {
  keys <- 
    aug_trip_data %>% 
    group_by(vin, contract_start_date) %>% 
    slice(1) %>% 
    group_by(vin) %>% 
    arrange(expo) %>% 
    slice(n()) %>% 
    select(vin, contract_start_date) %>% 
    ungroup()
  
  res <- left_join(keys, aug_trip_data, by = c("vin", "contract_start_date"))
  
  return(res)
}


