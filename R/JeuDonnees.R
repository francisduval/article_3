JeuDonnees <- 
  R6Class(
    "JeuDonnees",
    
    public =
      list(
        classic_ml_data = NULL,
        tele_ml_data = NULL,
        nn_data = NULL,
        
        initialize = function(data) {
          self$classic_ml_data <- 
            data %>% 
            group_by(vin) %>% 
            slice(1) %>% 
            select(vin, expo:years_licensed, claim_ind_cov_1_2_3_4_5_6) %>% 
            ungroup()
          
          self$tele_ml_data <- 
            data %>% 
            mutate(
              weekday = weekdays(date_start, abbreviate = T),
              is_weekend = weekday %in% c("Sat", "Sun"),
              distance_night_trip = (time_start >= as_hms("00:00:00") & time_start < as_hms("06:00:00")) * distance,
              distance_noon_trip = (time_start >= as_hms("11:00:00") & time_start < as_hms("14:00:00")) * distance,
              distance_evening_trip = (time_start >= as_hms("20:00:00") & time_start <= as_hms("23:59:59")) * distance,
              distance_peak_morning_trip = ((time_start >= as_hms("07:00:00") & time_start < as_hms("09:00:00")) & !is_weekend) * distance,
              distance_peak_evening_trip = ((time_start >= as_hms("17:00:00") & time_start < as_hms("20:00:00")) & !is_weekend) * distance,
              distance_mon_to_thu = (weekday %in% c("Mon", "Tue", "Wed", "Thu")) * distance,
              distance_fri_sat = (weekday %in% c("Fri", "Sat")) * distance,
              distance_sun = (weekday == "Sun") * distance
            ) %>% 
            rename(
              trip_avg_speed = avg_speed,
              trip_distance = distance,
              trip_duration = duration,
              trip_max_speed = max_speed
            ) %>% 
            group_by(vin) %>% 
            summarise(
              avg_daily_distance        = sum(trip_distance) / 365.25,
              avg_daily_nb_trips        = n() / 365.25,
              med_trip_avg_speed        = median(trip_avg_speed),
              med_trip_distance         = median(trip_distance),
              med_trip_max_speed        = median(trip_max_speed),
              max_trip_max_speed        = max(trip_max_speed),
              prop_long_trip            = sum(trip_distance > 100) / n(),
              frac_expo_night           = sum(distance_night_trip) / sum(trip_distance),
              frac_expo_noon            = sum(distance_noon_trip) / sum(trip_distance),
              frac_expo_evening         = sum(distance_evening_trip) / sum(trip_distance),
              frac_expo_peak_morning    = sum(distance_peak_morning_trip) / sum(trip_distance),
              frac_expo_peak_evening    = sum(distance_peak_evening_trip) / sum(trip_distance),
              frac_expo_mon_to_thu      = sum(distance_mon_to_thu) / sum(trip_distance),
              frac_expo_fri_sat         = sum(distance_fri_sat) / sum(trip_distance),
              claim_ind_cov_1_2_3_4_5_6 = first(claim_ind_cov_1_2_3_4_5_6)
            ) %>% 
            ungroup()
          
          compute_proportion <- function(time_start_vec, time_end_vec, nb_sec = 3600) {
            vv <- vector(mode = "numeric", length = 86400)
            n <- length(time_start_vec)
            
            for(i in 1:n) {
              ind <- seq(time_start_vec[i], time_end_vec[i])
              vv[ind] <- vv[ind] + 1
            }
            
            tot_sec <- map_dbl(1:(86400 / nb_sec), ~ sum(vv[(nb_sec * (. - 1) + 1):(nb_sec * .)]))
            res <- tot_sec / sum(tot_sec)
            
            return(res)
          }
          
          self$nn_data <-
            data %>%
            group_by(vin) %>%
            summarise(
              prop = list(compute_proportion(time_start, time_end)),
              distance = sum(distance),
              claim_ind_cov_1_2_3_4_5_6 = first(claim_ind_cov_1_2_3_4_5_6)
            ) %>%
            unnest(cols = c(prop)) %>%
            mutate(names = rep(glue("t_{1:24}"), nrow(.) / 24)) %>%
            select(vin, prop, names, claim_ind_cov_1_2_3_4_5_6, distance) %>%
            pivot_wider(names_from = names, values_from = prop)
        }
      )
  )
