keep_pol_k_veh <- function(tele_contract_data, k) {
  veh_once <- 
    tele_contract_data %>% 
    group_by(policy_id, vin) %>% 
    slice(1) %>% 
    ungroup()
  
  pol_to_keep <- 
    veh_once %>% 
    group_by(policy_id) %>% 
    count() %>% 
    filter(n >= k) %>% 
    pull(policy_id)
  
  res <- filter(veh_once, policy_id %in% pol_to_keep)
  return(res)
}
