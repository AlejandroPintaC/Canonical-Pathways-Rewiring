summary_distances <- data.frame(
  Pathway            = c("Neurexins & Neuroligins", "Synaptogenesis", 
                         "GABAergic", "SNARE"),
  Observed_AD        = c(obs_AD_NN,       obs_AD_Synap, 
                         obs_AD_GABAergic, obs_AD_SNARE),
  Observed_Control   = c(obs_ctrl_NN,       obs_ctrl_Synap,
                         obs_ctrl_GABAergic, obs_ctrl_SNARE),
  Null_mean_AD       = c(mean(null_AD_NN,      na.rm = TRUE),
                         mean(null_AD_Synap,   na.rm = TRUE),
                         mean(null_AD_GABAergic, na.rm = TRUE),
                         mean(null_AD_SNARE,   na.rm = TRUE)),
  Z_score_AD         = c(z_AD_NN,       z_AD_Synap,
                         z_AD_GABAergic, z_AD_SNARE),
  P_value_AD         = c(pval_AD_NN_2t,       pval_AD_Synap_2t,
                         pval_AD_GABAergic_2t, pval_AD_SNARE_2t),
  Z_score_Control    = c(z_ctrl_NN,       z_ctrl_Synap,
                         z_ctrl_GABAergic, z_ctrl_SNARE),
  P_value_Control    = c(pval_ctrl_NN_2t,       pval_ctrl_Synap_2t,
                         pval_ctrl_GABAergic_2t, pval_ctrl_SNARE_2t),
  N_permutations     = 1000
)

print(summary_distances)

write.csv(
  summary_distances,
  file = file.path(ruta_final, "summary_mean_distances_all_pathways.csv"),
  row.names = FALSE
)
