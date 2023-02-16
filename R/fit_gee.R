fit_gee <- function(data, formula, corstr) {
  data <- mutate(data, claim_ind_cov_1_2_3_4_5_6 = as.numeric(claim_ind_cov_1_2_3_4_5_6) - 1)
  
  geeglm(
    formula = formula,
    family = binomial,
    data = data,
    id = policy_id,
    corstr = corstr
  )
}
