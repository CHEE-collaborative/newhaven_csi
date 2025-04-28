################################################################################
# Sourced from https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R

################################################################################
format_pred_mod <- function(pred.csi) {
  fit.table.csi <- as.data.frame(pred.csi$matRRfit)
  colnames(fit.table.csi) <- paste0("rr_", colnames(fit.table.csi))
  fit.table.csi <- fit.table.csi %>% mutate(csi = as.numeric(row.names(fit.table.csi)))
  # 3d.ii Extract 95% CI
  lci.table.csi <- as.data.frame(pred.csi$matRRlow)
  colnames(lci.table.csi) <- paste0("lci_", colnames(lci.table.csi))
  uci.table.csi <- as.data.frame(pred.csi$matRRhigh)
  colnames(uci.table.csi) <- paste0("uci_", colnames(uci.table.csi))
  ## plot
  plot.csi_mod <- data.frame(fit.table.csi, lci.table.csi, uci.table.csi)
  names(plot.csi_mod) <- c("pe", "csi", "lci", "uci")
  return(plot.csi_mod)
}
