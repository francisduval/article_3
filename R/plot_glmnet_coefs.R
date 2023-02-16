plot_glmnet_coefs <- function(fitted_wf, title = NULL, subtitle = NULL, caption = T) {
  df <- 
    fitted_wf %>% 
    extract_fit_parsnip() %>% 
    tidy() %>% 
    filter(term != "(Intercept)")
  
  df_non_zero <- 
    df %>% 
    filter(estimate != 0)
  
  nb_zero_coef <- nrow(df) - nrow(df_non_zero)
  
  p <- 
    df_non_zero %>% 
    mutate(Sign = if_else(estimate > 0, "+", "-")) %>% 
    mutate(abs_estimate = abs(estimate)) %>% 
    mutate(term = fct_reorder(term, abs_estimate)) %>% 
    ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
    geom_col(alpha = 0.7) +
    xlab(NULL) +
    ylab("Absolute value of coefficient") +
    scale_fill_manual(values = c("#a61d21", "#00743F")) +
    coord_flip()
  
  if(caption) {
    p + labs(caption = glue("Note: {nb_zero_coef} other coefficients are zero"))
  }
}