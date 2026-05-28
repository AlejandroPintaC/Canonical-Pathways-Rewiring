# Extraer membresía de comunidades
membership_AD   <- V(grafo_AD)$community
membership_ctrl <- V(grafo_ctrl)$community

names(membership_AD)   <- V(grafo_AD)$name
names(membership_ctrl) <- V(grafo_ctrl)$name

# Función para calcular Jaccard entre dos vectores de membresía
calcular_jaccard_matrix <- function(memb_AD, memb_ctrl) {
  comunidades_AD   <- sort(unique(memb_AD))
  comunidades_ctrl <- sort(unique(memb_ctrl))
  
  mat <- matrix(0, 
                nrow = length(comunidades_AD), 
                ncol = length(comunidades_ctrl),
                dimnames = list(comunidades_AD, comunidades_ctrl))
  
  for (i in comunidades_AD) {
    genes_i <- names(memb_AD[memb_AD == i])
    for (j in comunidades_ctrl) {
      genes_j    <- names(memb_ctrl[memb_ctrl == j])
      intersec   <- length(intersect(genes_i, genes_j))
      union_size <- length(union(genes_i, genes_j))
      if (union_size > 0) mat[as.character(i), as.character(j)] <- intersec / union_size
    }
  }
  mat
}

# Valor observado
jaccard_obs <- calcular_jaccard_matrix(membership_AD, membership_ctrl)
obs_jaccard <- mean(apply(jaccard_obs, 1, max))

# Parámetros
n_perm <- 1000
set.seed(42)

null_jaccard <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  if (i %% 100 == 0) cat(sprintf("Permutación %d / %d\n", i, n_perm))
  memb_ctrl_perm        <- sample(membership_ctrl)
  names(memb_ctrl_perm) <- names(membership_ctrl)
  jac_perm              <- calcular_jaccard_matrix(membership_AD, memb_ctrl_perm)
  null_jaccard[i]       <- mean(apply(jac_perm, 1, max))
}

# Estadísticos
pval_jaccard    <- mean(null_jaccard >= obs_jaccard, na.rm = TRUE)
pval_jaccard_2t <- 2 * min(pval_jaccard, 1 - pval_jaccard)
ci_jaccard      <- quantile(null_jaccard, probs = c(0.025, 0.975), na.rm = TRUE)
z_jaccard       <- (obs_jaccard - mean(null_jaccard, na.rm = TRUE)) / sd(null_jaccard, na.rm = TRUE)

summary_permutation_jaccard <- data.frame(
  Observed_mean_max_jaccard = obs_jaccard,
  Null_mean                 = mean(null_jaccard, na.rm = TRUE),
  Null_SD                   = sd(null_jaccard,   na.rm = TRUE),
  CI_lower_95               = ci_jaccard[1],
  CI_upper_95               = ci_jaccard[2],
  Z_score                   = z_jaccard,
  P_value_2tailed           = pval_jaccard_2t,
  N_permutations            = n_perm
)

print(summary_permutation_jaccard)

write.csv(
  summary_permutation_jaccard,
  file = file.path(ruta_final, "permutation_test_Jaccard.csv"),
  row.names = FALSE
)
