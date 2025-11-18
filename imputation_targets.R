imputation_targets <- list(
  
  # single imputation for variables with <10 missing obs
  tar_target(df3, single_imputation(df2)),
  
  # Remove prevalent users from main dataset
  tar_target(df4, remove_prevalent_users(df3)),
  
  # Remove death or prevalent dementia at start of follow-up
  tar_target(df5, remove_death_dem(df4)),
  
  # save mean and SD to undo standardisation
  tar_target(mean_sd, save_mean_sd(df5)),
  
  # standardise to mean 0 SD 1
  tar_target(df6, standardise(df5)),
  
  # Take bootstrap samples
  tar_rep(
    boot_samples,
    get_boot_samples(df6),
    reps = 1,
    batches = 250
  ),
  
  # Impute for each bootstrap sample
  tar_target(
    imp_list,
    fit_mi(boot_samples, m = 2, maxit = 10, mean_sd = mean_sd),
    pattern = map(boot_samples)
  ),
  
  # CART imputation sensitivity analysis 
  tar_target(
    imp_list_cart,
    fit_mi_cart(boot_samples, m = 2, maxit = 10, mean_sd = mean_sd),
    pattern = map(boot_samples)
  )
)