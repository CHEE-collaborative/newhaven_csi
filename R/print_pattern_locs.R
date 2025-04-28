################################################################################
# Sourced from https://github.com/jaime-benavides/community_severance_nyc/blob/main/code/functions/functions.R

################################################################################
print_patterns_loc <- function(pats, colgroups = NULL, n = 1:6, pat_type = "pat",
                               title = "", size_line = 1, size_point = 1) {
  if (!is.null(colgroups)) {
    colgroups <- colgroups %>% dplyr::rename(chem = !!names(colgroups)[1])
  } else {
    colgroups <- data.frame(chem = rownames(pats), group = "1")
  }
  if (n > ncol(pats)) {
    n <- ncol(pats)
  }
  grouping <- names(colgroups)[2]
  colnames(pats) <- paste0(pat_type, stringr::str_pad(1:ncol(pats),
    width = 2, pad = "0", side = "left"
  ))
  pats.df <- pats %>%
    tibble::as_tibble() %>%
    dplyr::mutate(chem = colgroups[[1]]) %>%
    tidyr::pivot_longer(-chem, names_to = "pattern", values_to = "loading") %>%
    dplyr::right_join(., colgroups, by = "chem")
  pats.df$chem <- factor(as.character(pats.df$chem), levels = unique(as.character(pats.df$chem)))
  loadings <- pats.df %>%
    dplyr::filter(pattern %in% paste0(
      pat_type,
      stringr::str_pad(n, width = 2, pad = "0", side = "left")
    )) %>%
    ggplot(aes(x = chem, y = loading, color = !!sym(grouping))) +
    geom_point(size = size_point) +
    geom_segment(aes(yend = 0, xend = chem), size = size_line) +
    facet_wrap(~pattern) +
    theme_bw() +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 12), legend.title = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14), strip.background = element_rect(fill = "white"),
      axis.title.x = element_blank(), axis.title.y = element_blank()
    ) +
    geom_hline(yintercept = 0, size = 0.2) #+ ggtitle(title)
  loadings
}
