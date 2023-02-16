get_two_vins_per_policy <- function(tele_contract_data) {
  data_first_contract <- 
    tele_contract_data %>% 
    group_by(policy_id, vin) %>% 
    slice(1)
  
  policies_to_keep <- 
    data_first_contract %>% 
    group_by(policy_id) %>% 
    count() %>% 
    filter(n != 1) %>% 
    pull(policy_id)
  
  res <- 
    data_first_contract %>% 
    filter(policy_id %in% policies_to_keep) %>% 
    group_by(policy_id) %>% 
    slice(1:2) %>% 
    ungroup()

  return(res)
}