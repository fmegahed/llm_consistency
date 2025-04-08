miamired = '#C3142D'


# * Getting the Data ------------------------------------------------------

binary_classification_results = 
  readr::read_csv("../results/binary_analysis_df.csv") |> 
  dplyr::select(
    date, title, tickers:rep_5
  ) |> 
  tidyr::pivot_longer(
    cols = rep_1:rep_5,
    names_to = "rep",
    values_to = "llm_label"
  ) |>  
  dplyr::filter(chat_model != 'exaone-deep:2.4b') 



# * Stuff Needed for Coloring ---------------------------------------------

# * Chat Models -----------------------------------------------------------

# Assign alternating colors to chat_model labels
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
  "phi4:latest"               = "phi4:latest",
  "llama3.2:1B"               = "llama3.2:1B",
  "llama3.2:3B"               = "llama3.2:3B",
  "gemma3:1B"                 = "gemma3:1B",
  "gemma3:27B"                = "gemma3:27B",
  "deepseek-r1:1.5B"          = "deepseek-r1:1.5B",
  "deepseek-r1:7B"            = "deepseek-r1:7B",
  "command-r7b"               = "command-r7b",
  "command-r-plus-08-2024"    = "command-r-plus",
  "gpt-4o-mini-2024-07-18"    = "gpt-4o-mini",
  "gpt-4o-2024-11-20"         = "gpt-4o",
  "claude-3-5-haiku-20241022" = "claude-3-5-haiku",
  "claude-3-7-sonnet-20250219"= "claude-3-7-sonnet"
)



# Apply alternating color span tags
label_colors = c("Negative" = "#1F78B4", "Invalid" = "#B0B0B0", "Positive" = "#A6CEE3")


# * Plotting ---------------------------------------------------------------

plots = purrr::imap(chat_models, function(model, idx) {
  color = if (idx %% 2 == 1) "#808080"  else miamired
  model_short = model_abbrev[[model]]
  
  binary_classification_results |>
    dplyr::filter(chat_model == model) |>
    dplyr::mutate(
      llm_label = forcats::fct_expand(llm_label, "Invalid"),
      llm_label = forcats::fct_na_value_to_level(llm_label, "Invalid"),
      llm_label = forcats::fct_relevel(llm_label, "Positive", "Invalid", "Negative")
    ) |>
    dplyr::group_by(llm_label) |>
    dplyr::summarize(count = dplyr::n()) |>
    dplyr::mutate(percent = count / sum(count) * 100) |>
    ggplot2::ggplot( ggplot2::aes(x = llm_label, y = percent, fill = llm_label)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::geom_text( ggplot2::aes(label = sprintf("%.1f%%", percent)), 
                        vjust = -0.5, size = 3, fontface = "bold") +
    ggplot2::scale_fill_manual(values = label_colors) +
    ggplot2::scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 50),
      labels = scales::label_percent(scale = 1)
    ) +
    ggplot2::ggtitle(model_short) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title =  ggplot2::element_text(color = color, hjust = 0.5, face = "bold", size = 8),
      axis.title.x =  ggplot2::element_blank(),
      axis.title.y =  ggplot2::element_blank(),
      axis.text =  ggplot2::element_text(size = 8, face = 'bold'),
      legend.position = "none"
    )
})

# Combine plots using patchwork
patchwork::wrap_plots(plots, ncol = 2, axis_titles ='collect') +
  patchwork::plot_annotation(
    title = "Distribution of LLM Labels for Binary Classification",
    subtitle = "<span style='color:#808080'>Cheaper</span> vs. <span style='color:#C3142D'>more expensive (time, cost) </span> LLMs by company",
    theme = ggplot2::theme(
      plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 11),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 10),
      axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
      axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
      axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 7),
      axis.text.y = ggtext::element_markdown(hjust = 1, size = 7),
      strip.text = ggtext::element_markdown(face = "bold", size = 8),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none",
    )
  )

ggplot2::ggsave("C:\\users\\megahefm\\downloads\\label_dist.pdf", width = 4.5, height = 6)

