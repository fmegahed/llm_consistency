miamired = '#C3142D'

chat_model_colors = rep(c('#808080', '#C3142D'), 7)

mode_frequency = function(x, drop_na=F) {
  # return 0 if all values are NA
  if (all(is.na(x))) {
    return(0)
  }
  # calculate the frequency table
  freq_table = table(x)
  if (drop_na==F){
    # return the frequency of the most common value
    return(max(freq_table) / length(x))
  }else{
    return(max(freq_table)/sum(freq_table))
  }
}


# * Getting the Data ------------------------------------------------------

binary_classification_results = 
  readr::read_csv("C:\\users\\megahefm\\desktop\\binary_analysis_df.csv") |> 
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
  "phi4-mini"                = "phi4-mini",
  "phi4:latest"              = "phi4:latest",
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




# * Testing the Logic -----------------------------------------------------

test_df = tibble::tibble(
  rep_1 = c("Negative", "Positive", NA, "Positive"),
  rep_2 = c("Negative", "Positive", "Positive", NA),
  rep_3 = c("Negative", "Positive", NA, "Positive"),
  rep_4 = c("Negative", "Positive", "Positive", NA),
  rep_5 = c("Negative", "Positive", NA, "Positive")
)

test_df |>
  dplyr::rowwise() |>
  dplyr::mutate(percent_agreement = mode_frequency(dplyr::c_across(dplyr::starts_with("rep_")))) |>
  dplyr::ungroup()



# * Per Task and Model Labels ---------------------------------------------

binary_classification_results |>  
  tidyr::pivot_wider(
    names_from = rep,
    values_from = llm_label
  ) |> 
  dplyr::select(-date) -> binary_classification_results_wide

binary_analysis_df = 
  binary_classification_results_wide |>
  dplyr::rowwise() |>
  dplyr::mutate(
    percent_agreement = 100* mode_frequency(dplyr::c_across(starts_with("rep_")))
  ) |> 
  dplyr::ungroup()

agreement = 
  binary_analysis_df |> 
  dplyr::group_by(chat_model, percent_agreement) |> 
  dplyr::summarize(count = dplyr::n()) |>
  dplyr::ungroup() |> 
  dplyr::group_by(chat_model) |>
  dplyr::mutate(
    percent = 100 * (count / sum(count)),
    chat_model = model_abbrev[chat_model] |> as.factor(),
    chat_model = forcats::fct_relevel(
      chat_model, 
      "phi4-mini", "phi4:latest", "llama3.2:1B", "llama3.2:3B", 
      "gemma3:1B", "gemma3:27B", "deepseek-r1:1.5B", "deepseek-r1:7B", 
      "command-r7b", "command-r-plus", "gpt-4o-mini", "gpt-4o", 
      "claude-3-5-haiku", "claude-3-7-sonnet"
    )
  ) |>
  dplyr::ungroup()

agreement |> 
  ggplot2::ggplot(ggplot2::aes(x = percent_agreement, y = percent, fill = chat_model)) +
  ggplot2::geom_bar(stat = 'identity', color = 'black') +
  ggplot2::scale_fill_manual(values = chat_model_colors) +
  ggplot2::facet_wrap(~chat_model, ncol = 2) +
  ggplot2::labs(y = "Percentage") +
  ggplot2::scale_x_continuous(breaks = seq(0,100,20)) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 150), breaks = seq(0,100,50)) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", percent)), 
                     vjust = -0.5, size = 2.75, fontface = "bold") +
  ggplot2::labs(
    title = "Distribution of NA-Penalized Agreement Across Articles by LLM" |> 
      stringr::str_wrap(width = 50),
    x = "Percent Agreement (NA Penalty)",
    y = "Percent of Articles with Agreement (NA Penalty) Denoted by X-Axis" |> 
      stringr::str_wrap(width = 50),
    fill = "LLM Model"
    ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 10),
    axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 7, face = 'bold'),
    axis.text.y = ggtext::element_markdown(hjust = 1, size = 7, face = 'bold'),
    strip.text = ggtext::element_markdown(face = "bold", size = 8),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "none",
  ) 


# * Plot List -------------------------------------------------------------

chat_model_colors = setNames(chat_model_colors, unique(agreement$chat_model) )

# Split data by chat_model and get the names for titles
chat_model_groups = agreement |> 
  dplyr::group_by(chat_model) |> 
  dplyr::group_split()

chat_model_names = agreement |> 
  dplyr::group_by(chat_model) |> 
  dplyr::group_keys() |> 
  dplyr::pull(chat_model)

# Create a list of plots with the chat_model as the title
plot_list = purrr::map2(chat_model_groups, chat_model_names, ~{
  title_color = chat_model_colors[[.y]]
  title_html = glue::glue("<span style='color:{title_color}'>{.y}</span>")
  
  ggplot2::ggplot(.x, ggplot2::aes(x = percent_agreement, y = percent, fill = chat_model)) +
    ggplot2::geom_bar(stat = 'identity', color = 'black') +
    ggplot2::scale_fill_manual(values = chat_model_colors) +
    ggplot2::labs(
      title = title_html,
      x = "Percent Agreement within a LLM (NA Penalty)",
      y = 'Percentage of Articles',
      fill = "LLM Model"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      limits = c(0, 160),
      breaks = seq(0, 100, 50)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", percent)),
      vjust = -0.5,
      size = 2.5,
      fontface = "bold"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
      axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
      axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
      axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 7, face = "bold"),
      axis.text.y = ggtext::element_markdown(hjust = 1, size = 7, face = "bold"),
      strip.text = ggtext::element_markdown(face = "bold", size = 8),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )
})


patchwork::wrap_plots(plot_list, ncol = 2, axis_titles ='collect', axes = 'keep') +
  patchwork::plot_annotation(
    title = "Distribution of NA-Penalized Agreement Across Articles by LLM",
    subtitle = "<span style='color:#808080'>Cheaper</span> vs. <span style='color:#C3142D'>more expensive (time, cost) </span> LLMs by company",
    theme = ggplot2::theme(
      plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
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

ggplot2::ggsave("C:\\users\\megahefm\\downloads\\agreement_dist.pdf", width = 4.5, height = 6.5)
