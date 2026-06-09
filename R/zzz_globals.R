# Define global variables for non-standard evaluation (NSE) to satisfy R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "X1", "X2", "cluster", "k", "eigenvalue", "type", "joint_var",
      "variance_explained", "iteration", "total_energy", "status",
      "energy_type", "value", "value_rescaled", "experiment", "auc_nsa",
      "auc_pca", "random_accuracy", "Param", "domainLambda", "Metric",
      "Value", "alignment", "recovery_r2", "cross_rv", "cross_modal",
      "recon_err", "k_true", "k_shared_true", ".", "joint_var_rescaled",
      "max_joint_var", "is_significant"
    )
  )
}
