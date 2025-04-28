################################################################################
# Sourced from https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R

################################################################################
plot_pred_mod <- function(mod_dta) {
  mynamestheme <- theme(
    plot.title = element_text(family = "Helvetica", hjust = 0.5, size = (18)),
    axis.title = element_text(family = "Helvetica", size = (25)),
    axis.text = element_text(family = "Helvetica", size = (18))
  )

  p <- ggplot() +
    geom_rug(aes(x = csi),
      data = mod_dta,
      sides = "b", length = grid::unit(0.02, "npc")
    ) +
    geom_ribbon(aes(ymin = lci, ymax = uci, x = csi),
      fill = "blue",
      alpha = 0.2, data = mod_dta
    ) +
    geom_line(aes(x = csi, y = pe), lwd = 1, data = mod_dta) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey", size = 1.5) +
    labs(y = "Motor vehicle collisions", x = "Community severance index") +
    theme_bw() +
    mynamestheme
  return(p)
}
