define_recipe_tele <- function(data) {
  recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = data) %>% 
    update_role(policy_id:contract_end_date, new_role = "ID") %>% 
    step_rm(commute_distance) %>% 
    step_impute_median(years_claim_free) %>% 
    step_other(all_nominal_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_YeoJohnson(all_predictors()) %>% 
    step_normalize(all_predictors())
}
