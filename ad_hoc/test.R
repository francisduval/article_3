JeuDonnees_train <- tar_read(JeuDonnees_train)




atd1c_train <- tar_read(atd1c_train, branches = 1)

# ===============================================================================================================================

jeu <- JeuDonnees$new(atd1c_train)
jeu$nn_data %>% glimpse()

jeu$d_dat %>% glimpse()
jeu$h_dat %>% glimpse()
jeu$p_dat %>% glimpse()
jeu$vmo_dat %>% glimpse()
jeu$vma_dat %>% glimpse()


