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