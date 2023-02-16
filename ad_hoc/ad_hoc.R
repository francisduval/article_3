tele_contract_data <- tar_read(tele_contract_data)

x <- 
  tele_contract_data %>% 
  group_by(policy_id, vin) %>% 
  slice(1)