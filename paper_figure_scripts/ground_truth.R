
pacman::p_load(grid, gridExtra, tidyverse)

# ---- Setup ----
chat_models = c(
  "phi4-mini",
  "phi4:latest",
  "llama3.2:1B",
  "llama3.2:3B", 
  "gemma3:1B",
  "gemma3:27B",
  "deepseek-r1:1.5B",
  "deepseek-r1:7B",
  "command-r7b",
  "command-r-plus-08-2024",
  "gpt-4o-mini-2024-07-18",
  "gpt-4o-2024-11-20",
  "claude-3-5-haiku-20241022",
  "claude-3-7-sonnet-20250219"
)

model_abbrev = c(
  "phi4-mini"                 = "phi4-mini",
  "phi4:latest"              = "phi4:latest",
  "llama3.2:1B"              = "llama3.2:1B",
  "llama3.2:3B"              = "llama3.2:3B",
  "gemma3:1B"                = "gemma3:1B",
  "gemma3:27B"               = "gemma3:27B",
  "deepseek-r1:1.5B"         = "deepseek-r1:1.5B",
  "deepseek-r1:7B"           = "deepseek-r1:7B",
  "command-r7b"              = "command-r7b",
  "command-r-plus-08-2024"   = "command-r-plus",
  "gpt-4o-mini-2024-07-18"   = "gpt-4o-mini",
  "gpt-4o-2024-11-20"        = "gpt-4o",
  "claude-3-5-haiku-20241022"= "claude-3-5-haiku",
  "claude-3-7-sonnet-20250219" = "claude-3-7-sonnet"
)

chat_model_colors = rep(c('#808080', '#C3142D'), length.out = length(chat_models))
chat_model_colors = stats::setNames(chat_model_colors, model_abbrev[chat_models])


model_labels <- purrr::map_chr( model_abbrev[chat_models], ~{
  if (grepl("^spacer", .x)) return("")
  glue::glue("<span style='color:{chat_model_colors[.x]}'><b>{.x}</b></span>")
})
names(model_labels) <- model_abbrev[chat_models]




# ---- Load Data ----
inter_df = 
  tibble::tribble(
    ~Model,             ~Accuracy, ~Acc_SE, ~TPR,    ~TPR_SE, ~TNR,    ~TNR_SE, ~PPV,    ~PPV_SE, ~F1,    ~F1_SE,
<<<<<<< Updated upstream
    "phi4-mini",        0.485,     0.001,   0.492,   0.004,   0.478,   0.002,   0.497,   0.001,  0.495,   0.002,
    "phi4:latest",      0.514,     0.002,   0.526,   0.001,   0.501,   0.004,   0.525,   0.002,  0.525,   0.001,
    "llama3.2:1B",      0.526,     0.001,   0.540,   0.000,   0.511,   0.003,   0.536,   0.002,  0.538,   0.001,
    "llama3.2:3B",      0.490,     0.000,   0.539,   0.005,   0.438,   0.005,   0.501,   0.000,  0.520,   0.002,
    "gemma3:1B",        0.511,     0.001,   0.462,   0.007,   0.563,   0.005,   0.525,   0.001,  0.491,   0.005,
    "gemma3:27B",       0.517,     0.002,   0.514,   0.001,   0.521,   0.004,   0.530,   0.002,  0.522,   0.001,
    "deepseek-r1:1.5B", 0.512,     0.003,   0.465,   0.007,   0.562,   0.001,   0.527,   0.003,  0.494,   0.005,
    "deepseek-r1:7B",   0.514,     0.002,   0.522,   0.003,   0.506,   0.001,   0.525,   0.002,  0.524,   0.002,
    "command-r-plus",   0.513,     0.004,   0.451,   0.004,   0.577,   0.005,   0.528,   0.004,  0.487,   0.004,
    "command-r7b",      0.514,     0.001,   0.557,   0.003,   0.469,   0.001,   0.524,   0.001,  0.540,   0.002,
    "gpt-4o",           0.509,     0.005,   0.462,   0.008,   0.558,   0.007,   0.523,   0.005,  0.491,   0.006,
    "gpt-4o-mini",      0.516,     0.003,   0.496,   0.005,   0.537,   0.004,   0.529,   0.003,  0.512,   0.004,
    "claude-3-5-haiku", 0.515,     0.003,   0.550,   0.005,   0.478,   0.001,   0.525,   0.002,  0.537,   0.004,
    "claude-3-7-sonnet",0.516,     0.002,   0.549,   0.004,   0.482,   0.002,   0.526,   0.002,  0.537,   0.003
=======
    "phi4-mini",        0.487,     0.002,   0.495,   0.002,   0.479,   0.006,   0.485,   0.002,  0.490,   0.000,
    "phi4:latest",      0.513,     0.003,   0.526,   0.001,   0.500,   0.004,   0.511,   0.003,  0.518,   0.002,
    "llama3.2:1B",      0.512,     0.000,   0.527,   0.001,   0.497,   0.001,   0.510,   0.000,  0.518,   0.000,
    "llama3.2:3B",      0.478,     0.000,   0.525,   0.003,   0.432,   0.002,   0.478,   0.000,  0.501,   0.002,
    "gemma3:1B",        0.489,     0.002,   0.442,   0.005,   0.535,   0.001,   0.486,   0.002,  0.463,   0.004,
    "gemma3:27B",       0.509,     0.002,   0.505,   0.003,   0.512,   0.001,   0.507,   0.002,  0.506,   0.002,
    "deepseek-r1:1.5B", 0.493,     0.003,   0.447,   0.010,   0.539,   0.004,   0.490,   0.003,  0.468,   0.007,
    "deepseek-r1:7B",   0.496,     0.002,   0.509,   0.002,   0.483,   0.001,   0.494,   0.002,  0.501,   0.002,
    "command-r-plus",   0.512,     0.004,   0.450,   0.002,   0.574,   0.007,   0.511,   0.005,  0.479,   0.003,
    "command-r7b",      0.507,     0.004,   0.553,   0.002,   0.462,   0.005,   0.505,   0.003,  0.527,   0.003,
    "gpt-4o-mini",      0.502,     0.002,   0.482,   0.004,   0.522,   0.003,   0.500,   0.003,  0.491,   0.003,
    "gpt-4o",           0.509,     0.002,   0.461,   0.007,   0.558,   0.005,   0.508,   0.002,  0.483,   0.004,
    "claude-3-5-haiku", 0.512,     0.003,   0.549,   0.006,   0.476,   0.002,   0.509,   0.003,  0.528,   0.004,
    "claude-3-7-sonnet",0.514,     0.003,   0.548,   0.005,   0.480,   0.002,   0.511,   0.002,  0.529,   0.004
>>>>>>> Stashed changes
  )

inter_df = inter_df |> 
  dplyr::rename(Accuracy_mean = Accuracy,
                TPR_mean = TPR,
                TNR_mean = TNR,
                PPV_mean = PPV,
                F1_mean = F1) |>
  dplyr::rename(Accuracy_se = Acc_SE,
                TPR_se = TPR_SE,
                TNR_se = TNR_SE,
                PPV_se = PPV_SE,
                F1_se = F1_SE)


inter_long = inter_df |> 
  tidyr::pivot_longer(
    cols = -Model,
    names_to = c("Metric", ".value"),
    names_sep = "_"
  ) |>
  dplyr::mutate(
    lower = mean - (se/sqrt(5)),
    upper = mean + (se/sqrt(5)),
    Model = factor(Model, levels = rev(model_abbrev[chat_models]))
    )

table_df <- inter_df |>
  dplyr::select(Model, Accuracy_mean, TPR_mean, TNR_mean, PPV_mean, F1_mean) %>%
  dplyr::rename(
    LLM = Model,
    ACC = Accuracy_mean,
    TPR = TPR_mean,
    TNR = TNR_mean,
    PPV = PPV_mean,
    F1 = F1_mean) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ sprintf("%.2f", .))
  )


# *Plot the Data ----------------------------------------------------------

inter_long |> 
  ggplot2::ggplot(ggplot2::aes(x = Model, y = mean, color = Model)) +
  ggplot2::geom_blank() +
  ggplot2::geom_point(size = 0.9, na.rm = TRUE) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lower, ymax = upper),
    width = 0.4,
    na.rm = TRUE
  ) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", lower), y = lower), size = 2.25, hjust = 1.2, na.rm = TRUE) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", upper), y = upper), size = 2.25, hjust = -0.2, na.rm = TRUE) +
  ggplot2::scale_color_manual(values = chat_model_colors, na.translate = FALSE) +
  ggplot2::scale_x_discrete(labels = model_labels) +
  ggplot2::scale_y_continuous(limits = c(0.375, 0.65), breaks = seq(0.4, .61, 0.1)) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ Metric, ncol = 2, scales = "fixed", axes = 'all') +
  ggplot2::labs(
    title = "Classification Performance vs. Actual Market Behavior",
    subtitle = "<span style='color:#808080'>Cheaper</span> vs. <span style='color:#C3142D'>more expensive (time, cost) </span> LLMs by company",
    x = NULL,
    y = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 8),
    axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 8),
    axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 8),
    axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 7, face = 'bold'),
    axis.text.y = ggtext::element_markdown(hjust = 1, size = 7, face = 'bold'),
    strip.text = ggtext::element_markdown(face = "bold", size = 8),
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "black"),
    axis.ticks = ggplot2::element_line(color = 'black')
  ) -> original_plot

original_plot

# * Adding a table in the empty space at the bottom right -----------------

# Custom theme
booktabs_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontface = "plain", cex = 0.55, hjust = 0, x = 0.05)
  ),
  colhead = list(
    fg_params = list(fontface = "bold", cex = 0.6, hjust = 0, x = 0.05),
    bg_params = list(fill = NA)  # No shading, like booktabs
  ),
  padding = unit(c(1.7, 1.7), "mm")  # Tighter padding
)

# Base table
table_grob <- gridExtra::tableGrob(table_df, rows = NULL, theme = booktabs_theme)
table_grob$heights <- ggplot2::unit(rep(1, nrow(table_grob)), "lines") * 0.5

# Add title row
title_grob <- grid::textGrob(
  "Validity with External Criterion",
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

model_colors <- rep(c("#808080", "#C3142D"), length.out = 14)  # matches inter_wide

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


ggplot2::ggsave("../figs/ground_truth.pdf", width = 4.5, height = 6.5)
