pacman::p_load(grid, gridExtra, tidyverse)
source("all_functions.R")

# ---- Setup ----
chat_models <- c(
  "phi4-mini",
  "llama3.2:1B",
  "gemma3:1B",                
  "deepseek-r1:1.5B",
  "command-r7b",
  "gpt-4o-mini-2024-07-18",
  "claude-3-5-haiku-20241022",
  "phi4:latest", 
  "llama3.2:3B", 
  "gemma3:27B",
  "deepseek-r1:7B",
  "command-r-plus-08-2024",
  "gpt-4o-2024-11-20",
  "claude-3-7-sonnet-20250219"
)

model_abbrev = c(
  "phi4-mini"                 = "phi4-mini",
  "llama3.2:1B"              = "llama3.2:1B",
  "gemma3:1B"                = "gemma3:1B",
  "deepseek-r1:1.5B"         = "deepseek-r1:1.5B",
  "command-r7b"              = "command-r7b",
  "gpt-4o-mini-2024-07-18"   = "gpt-4o-mini",
  "claude-3-5-haiku-20241022"= "claude-3-5-haiku",
  "phi4:latest"              = "phi4:latest",
  "llama3.2:3B"              = "llama3.2:3B",
  "gemma3:27B"               = "gemma3:27B",
  "deepseek-r1:7B"           = "deepseek-r1:7B",
  "command-r-plus-08-2024"   = "command-r-plus",
  "gpt-4o-2024-11-20"        = "gpt-4o",
  "claude-3-7-sonnet-20250219" = "claude-3-7-sonnet"
)

chat_model_colors = rep(c('#808080', '#C3142D'), each = 7)
chat_model_colors = stats::setNames(chat_model_colors, model_abbrev[chat_models])


model_labels <- purrr::map_chr( model_abbrev[chat_models], ~{
  if (grepl("^spacer", .x)) return("")
  glue::glue("<span style='color:{chat_model_colors[.x]}'><b>{.x}</b></span>")
})
names(model_labels) <- model_abbrev[chat_models]

alpha = (1-(1-.1)^(1/7))


# ---- Load Data ----
intra_df = readr::read_csv("./results/binary_reliability_metrics.csv") |>
  dplyr::mutate(
    lower_ci = coeff.val - stats::qnorm(1-alpha/2) * coeff.se,
    upper_ci = coeff.val + stats::qnorm(1-alpha/2)* coeff.se,
    abbrev = factor(model_abbrev[model]),
    abbrev = forcats::fct_relevel(abbrev, rev(model_abbrev[chat_models]) )
  )

intra_wide = intra_df |> 
  dplyr::select(abbrev, coeff.name, coeff.val) |> 
  tidyr::pivot_wider(names_from = coeff.name, values_from = coeff.val) |> 
  dplyr::arrange(dplyr::desc(abbrev)) 
colnames(intra_wide) <- c("LLM", "C", "F", 'AC', "BP", "K")


intra_wide = intra_wide |> 
  dplyr::select(LLM, AC, BP, C, F, K) |> 
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ ifelse(is.na(.x), "--", sprintf("%.2f", .x)))
  )


# *Plot the Data ----------------------------------------------------------

intra_df |> 
  ggplot2::ggplot(ggplot2::aes(x = abbrev, y = coeff.val)) +
  ggplot2::geom_blank() +
  ggplot2::geom_point(ggplot2::aes(color = abbrev), size = 1.5, na.rm = TRUE) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower_ci, ymax = upper_ci, color = abbrev),
    width = 0.5,
    na.rm = TRUE
  ) +
  # Annotate lower CI values
  ggplot2::geom_text(
    data = intra_df,
    ggplot2::aes(
      y = lower_ci,
      label = sprintf("%.3f", lower_ci),
      color = abbrev
    ),
    size = 2.25,
    hjust = 1.2,
    na.rm = TRUE
  ) +
  
  # Annotate upper CI values
  ggplot2::geom_text(
    data = intra_df,
    ggplot2::aes(
      y = upper_ci,
      label = sprintf("%.3f", upper_ci),
      color = abbrev
    ),
    size = 2.25,
    hjust = -0.2,
    na.rm = TRUE
  ) +
  ggplot2::scale_color_manual(values = chat_model_colors, na.translate = FALSE) +
  ggplot2::scale_x_discrete(labels = model_labels) +
  ggplot2::scale_y_continuous(limits = c(0.785, 1.053), breaks = seq(0.8, 1.0, 0.05)) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ coeff.name, ncol = 2, scales = "fixed", axes = 'all') +
  ggplot2::labs(
    title = "Intra-LLM Reliability",
    subtitle = "<span style='color:#808080'>Cheaper</span> vs. <span style='color:#C3142D'>more expensive (time, cost) </span> LLMs by company",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 7, face = 'bold'),
    axis.text.y = ggtext::element_markdown(hjust = 1, size = 7, face = 'bold'),
    strip.text = ggtext::element_markdown(face = "bold", size = 8),
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "black"),
    axis.ticks = ggplot2::element_line(color = 'black')
  ) -> original_plot



# * Adding a table in the empty space at the bottom right -----------------

# Custom theme
booktabs_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontface = "plain", cex = 0.6, hjust = 0, x = 0.05)
  ),
  colhead = list(
    fg_params = list(fontface = "bold", cex = 0.65, hjust = 0, x = 0.05),
    bg_params = list(fill = NA)  # No shading, like booktabs
  ),
  padding = unit(c(1.7, 1.7), "mm")  # Tighter padding
)

# Base table
table_grob <- gridExtra::tableGrob(intra_wide, rows = NULL, theme = booktabs_theme)
table_grob$heights <- ggplot2::unit(rep(1, nrow(table_grob)), "lines") * 0.5

# Add title row
title_grob <- grid::textGrob(
  "Intra-LLM Coefficient Estimates",
  gp = grid::gpar(fontface = "bold", fontsize = 9),
  x = 0.5, hjust = 0.5
)

# Add title row above the table
table_grob <- gtable::gtable_add_rows(table_grob, heights = unit(1.5, "lines"), pos = 0)
table_grob <- gtable::gtable_add_grob(
  table_grob,
  grobs = title_grob,
  t = 1, l = 1, r = ncol(table_grob)
)

table_width <- sum(table_grob$widths)
table_height <- sum(table_grob$heights)

padding <- ggplot2::unit(.5, "mm")

# Color code the table 
core_llm_indices <- which(
  table_grob$layout$name == "core-fg" & 
    table_grob$layout$l == 1  # column 1
)

model_colors <- rep(c("#808080", "#C3142D"), each=7)  # matches intra_wide

for (i in seq_along(core_llm_indices)) {
  grob_index <- core_llm_indices[i]
  original_gp <- table_grob$grobs[[grob_index]]$gp
  table_grob$grobs[[grob_index]]$gp <- modifyList(original_gp, gpar(col = model_colors[i]))
}

# Add border around entire table using grobTree
bordered_table <- grid::grobTree(
  grid::rectGrob(
    width = table_width+ padding,
    height = table_height + padding,
    gp = grid::gpar(fill = NA, lwd = 0.7, col ='black') 
  ),
  table_grob
)


final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(original_plot, 0, 0, 1, 1) +  # main plot takes full area
  cowplot::draw_grob(bordered_table, x = 0.742, y = 0.16, width = 0.01, height = 0.01)

final_plot


ggplot2::ggsave("./figs/intra_dist.pdf", width = 4.5, height = 6.5)
