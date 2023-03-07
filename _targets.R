# Importer les librairies =======================================================================================================
library(targets)
library(tarchetypes)
library(tidyverse)
library(tidymodels)
library(glmnet)
library(here)
library(fs)
library(qs)
library(lubridate)
library(fastDummies)
library(dtplyr)
library(hms)
library(stringr)
library(dtplyr)
library(conflicted)
library(gee)
library(multilevelmod)
library(geepack)
library(embed)
library(glue)
library(R6)

map <- purrr::map
filter <- dplyr::filter
select <- dplyr::select


# Lire les fonctions ============================================================================================================
walk(dir_ls("R"), source)


# Options et thème ==============================================================================================================
options(scipen = 999)
theme_set(theme_bw())


# Options =======================================================================================================================
tar_option_set(
  garbage_collection = T,
  memory = "transient",
  format = "qs",
  workspace_on_error = T,
  iteration = "list"
)


# Targets =======================================================================================================================
list(
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Préparation des données -----------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(contract_file, "data/Contrat_Nov2020.csv", format = "file"),
  tar_target(contract_data, prepare_contract_data(contract_file)),
  tar_target(claim_data, prepare_claim_data(contract_data)),
  tar_files_input(trip_files, list.files(here("data"), pattern = "TRIP_VIN", full.names = T), format  = "file"),
  tar_target(trip_data, clean_trip_file(trip_files), pattern = map(trip_files), iteration = "vector"),
  
  # ----------
  
  tar_target(aug_trip_data, join_contracts_claims_trips(contract_data, claim_data, trip_data), pattern = map(trip_data), iteration = "vector"),
  tar_target(aug_trip_data_1_contract, keep_one_contract(aug_trip_data), pattern = map(aug_trip_data), iteration = "vector"),
  
  # ----------
  
  tar_target(vins, unique(aug_trip_data_1_contract$vin), pattern = map(aug_trip_data_1_contract), iteration = "vector"),
  
  tar_target(vins_learn, vins[1:40000]),
  tar_target(vins_train, vins_learn[1:25000]),
  tar_target(vins_valid, vins_learn[25001:32500]),
  tar_target(vins_test, vins_learn[32501:40000]),
  tar_target(vins_confirm, vins[40001:49671]),
  
  # ----------
  
  tar_target(atd1c_learn, filter(aug_trip_data_1_contract, vin %in% vins_learn), pattern = map(aug_trip_data_1_contract), iteration = "vector"),
  tar_target(atd1c_train, filter(aug_trip_data_1_contract, vin %in% vins_train), pattern = map(aug_trip_data_1_contract), iteration = "vector"),
  tar_target(atd1c_valid, filter(aug_trip_data_1_contract, vin %in% vins_valid), pattern = map(aug_trip_data_1_contract), iteration = "vector"),
  tar_target(atd1c_test, filter(aug_trip_data_1_contract, vin %in% vins_test), pattern = map(aug_trip_data_1_contract), iteration = "vector"),
  tar_target(atd1c_confirm, filter(aug_trip_data_1_contract, vin %in% vins_confirm), pattern = map(aug_trip_data_1_contract), iteration = "vector"),
  
  # ----------
  
  tar_target(JeuDonnees_learn, JeuDonnees$new(atd1c_learn)),
  tar_target(JeuDonnees_train, JeuDonnees$new(atd1c_train)),
  tar_target(JeuDonnees_valid, JeuDonnees$new(atd1c_valid)),
  tar_target(JeuDonnees_test, JeuDonnees$new(atd1c_test)),
  tar_target(JeuDonnees_confirm, JeuDonnees$new(atd1c_confirm)),

  # ----------

  tar_target(
    train_df, 
    JeuDonnees_train$classic_ml_data %>% 
      left_join(JeuDonnees_train$tele_ml_data, by = "vin") %>% 
      left_join(JeuDonnees_train$nn_data, by = "vin")
  ),
  
  tar_target(
    valid_df, 
    JeuDonnees_valid$classic_ml_data %>% 
      left_join(JeuDonnees_valid$tele_ml_data, by = "vin") %>% 
      left_join(JeuDonnees_valid$nn_data, by = "vin")
  ),
  
  tar_target(train_valid_df, bind_rows(train_df, valid_df)),
  
  tar_target(
    test_df, 
    JeuDonnees_test$classic_ml_data %>% 
      left_join(JeuDonnees_test$tele_ml_data, by = "vin") %>% 
      left_join(JeuDonnees_test$nn_data, by = "vin")
  ),
  
  # ----------
  
  
  
  # ----------

  # tar_target(tele_contract_train, compute_telematics_summaries(atd1c_learn)),
  # tar_target(tele_contract_test, compute_telematics_summaries(atd1c_confirm)),
  
  # ---------- 
  
  tar_target(tele_contract_data, compute_telematics_summaries(aug_trip_data), pattern = map(aug_trip_data), iteration = "vector"),
  tar_target(k_vec, 1:5),
  tar_target(
    dat_ls,
    keep_pol_k_veh(tele_contract_data, k = k_vec),
    pattern = map(k_vec)
  ),
  
  # ---------- 
  
  tar_target(nn_data_time_of_day, make_nn_data(aug_trip_data, nb_sec = 900), pattern = map(aug_trip_data), iteration = "vector"),
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # Dépendance entre les véhicules d'une même police ----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------

  tar_target(data_two_vins_per_policy, get_two_vins_per_policy(tele_contract_data)),
  tar_target(rec, define_recipe(data_two_vins_per_policy)),
  tar_target(rec_tele, define_recipe_tele(data_two_vins_per_policy)),
  tar_target(folds, vfold_cv(data_two_vins_per_policy, v = 5, strata = claim_ind_cov_1_2_3_4_5_6)),
  tar_target(glmnet_fit, tune_train_glmnet(data_two_vins_per_policy, recipe = rec, resamples = folds)),
  tar_target(glmnet_tele_fit, tune_train_glmnet(data_two_vins_per_policy, recipe = rec_tele, resamples = folds)),

  
  # -----------------------------------------------------------------------------------------------------------------------------
  # GEE and GLM -----------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  tar_target(dat_split_ls, initial_split(dat_ls, strata = claim_ind_cov_1_2_3_4_5_6), pattern = map(dat_ls)),
  tar_target(dat_train_ls, training(dat_split_ls), pattern = map(dat_split_ls)),
  tar_target(dat_test_ls, testing(dat_split_ls), pattern = map(dat_split_ls)),

  tar_target(
    rec_ls,
    recipe(claim_ind_cov_1_2_3_4_5_6 ~ ., data = dat_train_ls) %>%
      step_rm(vin, contract_start_date, contract_end_date) %>%
      update_role(policy_id, new_role = "ID") %>%
      step_other(all_nominal_predictors(), threshold = 0.05) %>%
      step_lencode_glm(all_nominal_predictors(), outcome = "claim_ind_cov_1_2_3_4_5_6") %>%
      step_impute_bag(commute_distance, years_claim_free) %>%
      step_normalize(all_numeric_predictors()),
    pattern = map(dat_train_ls)
  ),

  tar_target(dat_train_baked_ls, bake(prep(rec_ls), new_data = dat_train_ls), pattern = map(rec_ls, dat_train_ls)),
  tar_target(dat_test_baked_ls, bake(prep(rec_ls), new_data = dat_test_ls), pattern = map(rec_ls, dat_test_ls)),

  tar_target(
    formules,
    list(
      class_tele = as.formula("claim_ind_cov_1_2_3_4_5_6 ~ expo + annual_distance + commute_distance + conv_count_3_yrs_minor + gender + marital_status + pmt_plan + veh_age + veh_use + years_claim_free + years_licensed + avg_daily_distance + avg_daily_nb_trips + med_trip_avg_speed + med_trip_distance + med_trip_max_speed + max_trip_max_speed + prop_long_trip + frac_expo_night + frac_expo_noon + frac_expo_evening + frac_expo_peak_morning + frac_expo_peak_evening + frac_expo_mon_to_thu + frac_expo_fri_sat"),
      class = as.formula("claim_ind_cov_1_2_3_4_5_6 ~ expo + annual_distance + commute_distance + conv_count_3_yrs_minor + gender + marital_status + pmt_plan + veh_age + veh_use + years_claim_free + years_licensed"),
      tele = as.formula("claim_ind_cov_1_2_3_4_5_6 ~ avg_daily_distance + avg_daily_nb_trips + med_trip_avg_speed + med_trip_distance + med_trip_max_speed + max_trip_max_speed + prop_long_trip + frac_expo_night + frac_expo_noon + frac_expo_evening + frac_expo_peak_morning + frac_expo_peak_evening + frac_expo_mon_to_thu + frac_expo_fri_sat")
    )
  ),

  fit <-
    tar_map(
      expand_grid(formule = c("class_tele", "class", "tele"), data = 1:5),
      tar_target(glm_fit, glm(formules[[formule]], family = binomial, data = dat_train_baked_ls[[data]])),
      tar_target(gee_fit, fit_gee(dat_train_baked_ls[[data]], formula = formules[[formule]], corstr = "exchangeable"))
    ),

  tar_combine(glm_fit_ls, fit[["glm_fit"]], command = list(!!!.x)),
  tar_combine(gee_fit_ls, fit[["gee_fit"]], command = list(!!!.x))
    
  # -----------------------------------------------------------------------------------------------------------------------------
  # RMarkdown -------------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------------------
  
  # tar_render(glm_vs_gee, "RMarkdown/glm_vs_gee/glm_vs_gee.Rmd"),
  # tar_render(bootstrap, "RMarkdown/bootstrap/bootstrap.Rmd"),
  # tar_render(gee_glm_nb_veh, "RMarkdown/gee_glm_nb_veh/gee_glm_nb_veh.Rmd"),
  # tar_render(correlation_entre_vehicules, "RMarkdown/correlation_entre_vehicules/correlation_entre_vehicules.Rmd"),
  # tar_render(nn_time_of_day, "RMarkdown/nn_time_of_day/nn_time_of_day.Rmd")
  
  # =============================================================================================================================
  
) 
  