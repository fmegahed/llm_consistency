pacman::p_load(grid, gridExtra, tidyverse)
source("all_functions.R")

# ---- Setup ----
chat_models = c(
  "Top 2 Cheaper",
  "Top 2 Expensive",
  "Top 3 Cheaper",
  "Top 3 Expensive",
  "Top 4 Cheaper",
  "Top 4 Expensive",
  "Top 5 Cheaper",
  "Top 5 Expensive",
  "Top 6 Cheaper",
  "Top 6 Expensive",
  "Top 7 Cheaper",
  "Top 7 Expensive"
)

model_abbrev = c(
  "Top 2 Cheaper"   = "Top 2 Cheaper",
  "Top 2 Expensive" = "Top 2 Expensive",
  "Top 3 Cheaper"   = "Top 3 Cheaper",
  "Top 3 Expensive" = "Top 3 Expensive",
  "Top 4 Cheaper"   = "Top 4 Cheaper",
  "Top 4 Expensive" = "Top 4 Expensive",
  "Top 5 Cheaper"   = "Top 5 Cheaper",
  "Top 5 Expensive" = "Top 5 Expensive",
  "Top 6 Cheaper"   = "Top 6 Cheaper",
  "Top 6 Expensive" = "Top 6 Expensive",
  "Top 7 Cheaper"   = "Top 7 Cheaper",
  "Top 7 Expensive" = "Top 7 Expensive"
)

chat_model_abbrev = c(
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


model_labels <- purrr::map_chr(model_abbrev[chat_models], ~{
  if (grepl("^spacer", .x)) return("")
  glue::glue("<span style='color:{chat_model_colors[.x]}'><b>{.x}</b></span>")
})

names(model_labels) <- model_abbrev[chat_models]


# ---- Load Data ----
intra_df = read_csv("../results/binary_reliability_metrics.csv") %>% 
  filter(coeff.name == "Krippendorff's Alpha") %>% 
  select(model, coeff.val, coeff.se)

intra_df$model <- factor(intra_df$model, levels = c("phi4-mini",                 
  "phi4:latest", "llama3.2:1B", "llama3.2:3B", "gemma3:1B",                
  "gemma3:27B", "deepseek-r1:1.5B", "deepseek-r1:7B",           
  "command-r7b", "command-r-plus-08-2024",
  "gpt-4o-mini-2024-07-18", "gpt-4o-2024-11-20",        
  "claude-3-5-haiku-20241022", "claude-3-7-sonnet-20250219")
  )

intra_df <- intra_df %>% 
  arrange(model) %>% 
  mutate(cost = rep(c("Cheaper", "Expensive"), 7)) %>% 
  select(cost, everything()) %>% 
  arrange(cost, desc(coeff.val))

binary_analysis_df <- read_csv("../results/binary_analysis_df.csv")

df_long <- binary_analysis_df %>%
  pivot_longer(cols = starts_with("rep"), names_to = "rep", values_to = "value")

inter_temp <- df_long %>%
  select(chat_model, value) %>%
  group_by(chat_model) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = chat_model, values_from = value) %>% 
  select(-id)

find_inter_metrics <- function(k, cost){
  models <- as.character(intra_df$model[intra_df$cost==cost])[1:k]
  temp <- inter_temp[models]
  temp$chat_model = paste("Top", k, cost)
  reliability_coefs(temp, 1:k) %>% 
    mutate(lower_ci = coeff.val - stats::qnorm(0.975) * coeff.se,
           upper_ci = coeff.val + stats::qnorm(0.975) * coeff.se) %>% 
    select(model, coeff.name, coeff.val, lower_ci, upper_ci)
}

# Define values to loop over
k_vals <- 2:7
cost_levels <- c("Cheaper", "Expensive")


# Create all combinations of k and cost
combinations <- expand.grid(k = 2:7, cost = c("Cheaper", "Expensive"), KEEP.OUT.ATTRS = FALSE)

# Apply the function for each combination
inter_df <- pmap_dfr(combinations, function(k, cost) {
  find_inter_metrics(k, cost) %>%
    mutate(k = k, cost = cost)
})

inter_df$abbrev <- factor(model_abbrev[inter_df$model])

intra_wide <- intra_df %>% 
  mutate(Rank = rep(paste("Top", 1:7), 2),
         LLM = chat_model_abbrev[model]
  ) %>% 
  select(Rank, LLM) 

inter_wide <- data.frame(Rank = 1:7,
                         Cheaper = intra_wide$LLM[1:7],
                         Expensive = intra_wide$LLM[8:14]) 


row.names(inter_wide) = NULL


# *Plot the Data -------NULL# *Plot the Data ----------------------------------------------------------

inter_df |> 
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
    data = inter_df,
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
    data = inter_df,
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
  ggplot2::scale_y_continuous(limits = c(0.615, 0.953), breaks = seq(0.65, 0.9, 0.05)) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ coeff.name, ncol = 2, scales = "fixed", axes = 'all') +
  ggplot2::labs(
    title = "Inter-LLM Reliability",
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
  padding = unit(c(4.5, 4.5), "mm")  # Tighter padding
)

# Base table
table_grob <- gridExtra::tableGrob(inter_wide, rows = NULL, theme = booktabs_theme)
table_grob$heights <- ggplot2::unit(rep(1, nrow(table_grob)), "lines")*0.7


# Add title row
title_grob <- grid::textGrob(
  "LLM Rank based on Kripendorff's Alpha",
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

# Find all core (body cell) grobs
core_indices <- which(table_grob$layout$name == "core-fg")

# Loop over each core cell and color by column and center the first column
for (i in core_indices) {
  col_num <- table_grob$layout[i, "l"]
  original_gp <- table_grob$grobs[[i]]$gp
  grob <- table_grob$grobs[[i]]  # current cell grob
  
  if (col_num == 1) {
    # Center-align column 1 (Rank)
    grob$hjust <- 0.5
    grob$x <- unit(0.5, "npc")
  } else if (col_num == 2) {
    # Cheaper LLM column — gray
    grob$gp <- modifyList(original_gp, gpar(col = "#808080"))
  } else if (col_num == 3) {
    # Expensive LLM column — red
    grob$gp <- modifyList(original_gp, gpar(col = "#C3142D"))
  }
  
  table_grob$grobs[[i]] <- grob
}


# Add border around entire table using grobTree
bordered_table <- grid::grobTree(
  grid::rectGrob(
    width = table_width + padding,
    height = table_height + padding,
    gp = grid::gpar(fill = NA, lwd = 0.7, col ='black') 
  ),
  table_grob
)


final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(original_plot, 0, 0, 1, 1) +  # main plot takes full area
  cowplot::draw_grob(bordered_table, x = 0.742, y = 0.16, width = 0.01, height = 0.01)

final_plot


ggplot2::ggsave("../figs/inter_dist.pdf", plot=final_plot, width = 5.5, height = 6.5)
