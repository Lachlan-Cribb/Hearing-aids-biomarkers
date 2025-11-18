descriptives_targets <- list(
  # Missing data summary
  tar_target(missing_data, get_missing(df2, df_elig), pattern = map(df_elig)),
  tar_target(missing_summary, get_missing_summary(missing_data)),
  
  # Missing data summary in survivors
  tar_target(missing_data_surv, get_missing(df2, df_main), pattern = map(df_main)),
  tar_target(missing_summary_surv, get_missing_summary(missing_data_surv)),
  
  # survival summary
  tar_target(surv_summary, get_surv_summary(df_elig, df_main)),
  
  # Sample size summary 
  tar_target(sample_size, get_sample_size(df_elig), pattern = map(df_elig)),
  tar_target(sample_size_summary, sample_size_summarise(sample_size)),
  
  # Sample size summary 
  tar_target(sample_size_surv, get_sample_size(df_main), pattern = map(df_main)),
  tar_target(sample_size_summary_surv, sample_size_summarise(sample_size_surv)),
  
  # Baseline characteristics
  tar_target(bl_table, get_baseline_table(df_elig), pattern = map(df_elig)),
  tar_target(bl_table_summary, baseline_table_summary(bl_table))
)