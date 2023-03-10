prepare_contract_data <- function(contract_file) {
  read_csv(contract_file, col_types = cols(.default = "c")) %>% 
    transmute(
      vin                    = VIN,
      policy_id              = POLICY_ID_M,
      contract_start_date    = as.Date(parse_date_time2(POLEFFDATE_M, "%d%b%Y!")),
      contract_end_date      = as.Date(parse_date_time2(POLEXPDATE_M, "%d%b%Y!")),
      expo_2                 = as.numeric(expo_Col),
      expo_3                 = as.numeric(expo_Comp),
      expo_4                 = as.numeric(expo_DCPD),
      expo_5                 = as.numeric(expo_Liab),
      annual_distance        = as.integer(ANNUALMILEAGE),
      commute_distance       = as.integer(COMMUTEDISTANCE),
      conv_count_3_yrs_minor = as.integer(CONVCOUNTMINOR3YRS),
      gender                 = factor(DRIVER_GENDER),
      marital_status         = fct_drop(factor(DRIVER_MARITALSTATUS)),
      pmt_plan               = fct_drop(factor(PAYMENTPLAN)),
      veh_age                = as.integer(VEHICLEAGE),
      veh_use                = fct_drop(factor(VEHICLEUSE)),
      years_claim_free       = as.integer(DRIVER_YEARSCLAIMFREE),
      years_licensed         = as.integer(DRIVER_YEARSLICENSED),
      first_claim_date       = as.Date(parse_date_time2(dateofloss1, "%d%b%Y:%H:%M:%OS")),
      second_claim_date      = as.Date(parse_date_time2(dateofloss2, "%d%b%Y:%H:%M:%OS")),
      third_claim_date       = as.Date(parse_date_time2(dateofloss3, "%d%b%Y:%H:%M:%OS")),
      fourth_claim_date      = as.Date(parse_date_time2(dateofloss4, "%d%b%Y:%H:%M:%OS")),
      first_claim_id         = claimid1,
      second_claim_id        = claimid2,
      third_claim_id         = claimid3,
      fourth_claim_id        = claimid4,
      first_claim_cov_1      = as.numeric(FirstClaim_cov1),
      first_claim_cov_2      = as.numeric(FirstClaim_cov2),
      first_claim_cov_3      = as.numeric(FirstClaim_cov3),
      first_claim_cov_4      = as.numeric(FirstClaim_cov4),
      second_claim_cov_1     = as.numeric(SecClaim_cov1),
      second_claim_cov_2     = as.numeric(SecClaim_cov2),
      second_claim_cov_3     = as.numeric(SecClaim_cov3),
      second_claim_cov_4     = as.numeric(SecClaim_cov4),
      third_claim_cov_1      = as.numeric(ThirdClaim_cov1),
      third_claim_cov_2      = as.numeric(ThirdClaim_cov2),
      third_claim_cov_3      = as.numeric(ThirdClaim_cov3),
      third_claim_cov_4      = as.numeric(ThirdClaim_cov4),
      fourth_claim_cov_1     = as.numeric(FourthClaim_cov1),
      fourth_claim_cov_2     = as.numeric(FourthClaim_cov2),
      fourth_claim_cov_3     = as.numeric(FourthClaim_cov3),
      fourth_claim_cov_4     = as.numeric(FourthClaim_cov4),
      first_claim_cost_1     = as.numeric(FirstClaim_loss1),
      first_claim_cost_2     = as.numeric(FirstClaim_loss2),
      first_claim_cost_3     = as.numeric(FirstClaim_loss3),
      first_claim_cost_4     = as.numeric(FirstClaim_loss4),
      second_claim_cost_1    = as.numeric(SecClaim_loss1),
      second_claim_cost_2    = as.numeric(SecClaim_loss2),
      second_claim_cost_3    = as.numeric(SecClaim_loss3),
      second_claim_cost_4    = as.numeric(SecClaim_loss4),
      third_claim_cost_1     = as.numeric(ThirdClaim_loss1),
      third_claim_cost_2     = as.numeric(ThirdClaim_loss2),
      third_claim_cost_3     = as.numeric(ThirdClaim_loss3),
      third_claim_cost_4     = as.numeric(ThirdClaim_loss4),
      fourth_claim_cost_1    = as.numeric(FourthClaim_loss1),
      fourth_claim_cost_2    = as.numeric(FourthClaim_loss2),
      fourth_claim_cost_3    = as.numeric(FourthClaim_loss3),
      fourth_claim_cost_4    = as.numeric(FourthClaim_loss4)
    ) %>% 
    arrange(vin, contract_start_date) %>% 
    filter(gender != "Unknown") %>% 
    mutate(gender = fct_drop(gender)) %>% 
    filter_at(vars(starts_with("expo_")), all_vars(near(., expo_2, tol = 0.01))) %>%
    rename(expo = expo_2) %>% 
    select(-starts_with("expo_")) %>% 
    filter(expo > 0)
}
