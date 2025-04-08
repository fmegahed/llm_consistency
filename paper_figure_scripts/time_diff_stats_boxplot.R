miamired = '#C3142D'

results = readr::read_csv("../results/binary_classification_results.csv")



# * Summary Table ---------------------------------------------------------

results |>  
  dplyr::filter(chat_model != 'exaone-deep:2.4b') |> 
  dplyr::mutate(
    chat_model = forcats::fct_relevel(
      chat_model,
      "claude-3-7-sonnet-20250219",
      "claude-3-5-haiku-20241022",
      
      "gpt-4o-2024-11-20",
      "gpt-4o-mini-2024-07-18",
      
      "command-r-plus-08-2024",
      "command-r7b",
      
      "deepseek-r1:7B",
      "deepseek-r1:1.5B",
      
      "gemma3:27B",
      "gemma3:1B",
      
      "llama3.2:3B", 
      "llama3.2:1B",
      
      'phi4:latest',
      'phi4-mini'
    )
  ) |> 
  dplyr::group_by(chat_model) |> 
  dplyr::mutate(
    time_diff = chat_date - dplyr::lag(chat_date)
  ) |> dplyr::select(chat_model, chat_date, time_diff) |> 
  dplyr::summarise(
    n = dplyr::n(),
    min_t = min(time_diff, na.rm = TRUE),
    q25_t = quantile(time_diff, 0.25, na.rm = TRUE),
    mean_t = mean(time_diff, na.rm = TRUE),
    med_t = median(time_diff, na.rm = TRUE),
    q75_t = quantile(time_diff, 0.75, na.rm = TRUE),
    p95_t = quantile(time_diff, 0.95, na.rm = TRUE),
    max_t = max(time_diff, na.rm = TRUE),
  ) |> 
  dplyr::mutate( 
    color = c(rep(c('exp', 'cheap'), 7) ),
    company = c('Anthropic', 'Anthropic', 'Cohere', 'Cohere', 'DeepSeek', 'DeepSeek',
                'Google', 'Google', 'OpenAI', 'OpenAI', 'Meta', 'Meta', 'Microsoft', 'Microsoft')
    ) -> 
  summary_table

summary_table



# * Plotting the box plot -------------------------------------------------


# create "fake" spacer levels for spacing the boxplot:
model_levels = c( "spacer-0",
  "claude-3-7-sonnet", "claude-3-5-haiku",  "spacer-2",
  "gpt-4o",            "gpt-4o-mini",       "spacer-10a", "spacer-10b",
  "command-r-plus",    "command-r7b",       "spacer-4",
  "deepseek-r1:7B",    "deepseek-r1:1.5B",  "spacer-6",
  "gemma3:27B",        "gemma3:1B",         "spacer-8",
  "llama3.2:3B",       "llama3.2:1B",       "spacer-12",
  "phi4:latest",       "phi4-mini",         "spacer-14"
)

# boxplot data:
results |>  
  dplyr::left_join(summary_table, by = "chat_model") |>
  dplyr::mutate(
    time_diff = (chat_date - dplyr::lag(chat_date)) |> 
      stringr::str_remove_all(" secs") |> as.numeric(),
    chat_model = forcats::fct_recode(
      chat_model,
      `claude-3-7-sonnet` = "claude-3-7-sonnet-20250219",
      `claude-3-5-haiku` = "claude-3-5-haiku-20241022",
      
      `command-r-plus` = "command-r-plus-08-2024",
      
      `gpt-4o` = "gpt-4o-2024-11-20",
      `gpt-4o-mini` = "gpt-4o-mini-2024-07-18"
    ) |> 
      factor(levels = model_levels)
  ) |> 
  na.omit() -> boxplot_df

# annotation df:
label_df = boxplot_df |>
  dplyr::group_by(chat_model) |>
  dplyr::summarise(
    min = round(quantile(time_diff, 0)),
    med = round(quantile(time_diff, 0.5)),
    max = round(quantile(time_diff, 1)),
    color = dplyr::first(color),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(
    cols = c(min, med, max),
    names_to = "stat",
    values_to = "label"
  ) |>
  dplyr::arrange(factor(chat_model, levels = model_levels)) |>
  dplyr::mutate(
    group_index = rep(seq_len(length(unique(chat_model))), each = 3),
    base_nudge = ifelse(group_index %% 2 == 1, -1, 1),
    # Apply tighter nudge for min/max, looser for median
    y_nudge = dplyr::case_when(
      stat == "med" ~ base_nudge * 0.725,
      stat %in% c("min", "max") ~ base_nudge * 0.5
    )
  ) |> 
  dplyr::filter( !(chat_model == "llama3.2:1B" & stat == 'min') )


# data for the rectangle boxes:
proprietary_models = c("claude-3-7-sonnet", "claude-3-5-haiku", "gpt-4o", "gpt-4o-mini")
y_indices = which(model_levels %in% proprietary_models)
y_min = min(y_indices) - 0.5
y_max = max(y_indices) + 0.5

open_api = c("command-r-plus")
y_indices2 = which(model_levels %in% open_api)
y_min2 = min(y_indices2) - 0.5
y_max2 = max(y_indices2) + 0.5

ollama_models = c(
  'command-r7b', 'deepseek-r1:7B', 'deepseek-r1:1.5B', 'gemma3:27B', 'gemma3:1B',
  'llama3.2:3B', 'llama3.2:1B', 'phi4:latest', 'phi4-mini'
)
y_indices3 = which(model_levels %in% ollama_models)
y_min3 = min(y_indices3) - 0.5
y_max3 = max(y_indices3) + 0.5


# colors for ylabels
color_lookup = boxplot_df |>
  dplyr::distinct(chat_model, color) |>
  dplyr::filter(!grepl("^spacer", chat_model)) |>
  dplyr::mutate(
    label_colored = dplyr::case_when(
      color == "exp" ~ paste0("<span style='color:#C3142D'>", chat_model, "</span>"),
      color == "cheap" ~ paste0("<span style='color:#808080'>", chat_model, "</span>"),
      TRUE ~ chat_model
    )
  )

model_labels = setNames(color_lookup$label_colored, color_lookup$chat_model)

# Add empty labels for spacers
spacer_labels = rep("", sum(grepl("^spacer", model_levels)))
names(spacer_labels) = model_levels[grepl("^spacer", model_levels)]

# Merge
full_labels = c(model_labels, spacer_labels)
full_labels = full_labels[model_levels]  # Reorder to match axis


# plot:
boxplot_df |>
  dplyr::mutate(chat_model = factor(chat_model, levels = model_levels)) |> 
  ggplot2::ggplot(ggplot2::aes(y = chat_model, x = time_diff)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = color), size =0.5) +  
  
  # adding blanks between pairs of models
  ggplot2::geom_blank(data = data.frame(chat_model = model_levels, time_diff = NA)) + 
  
  # use ggplot2::stat_summary to annotate the max for each model
  ggplot2::geom_text(
    data = label_df,
    ggplot2::aes(x = label, y = chat_model, label = label, color = color),
    position = ggplot2::position_nudge(y = label_df$y_nudge),
    size = 2.75,
  ) +
  
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_discrete(
    limits = model_levels,
    labels = full_labels
  ) +
  ggplot2::scale_color_manual(values = c("exp" = "#C3142D", "cheap" = "#808080")) +
  
  ggplot2::annotate("rect",
             xmin = 0.9, xmax = 1200,
             ymin = y_min-0.8, ymax = y_max+0.6,
             alpha = 0.02,
             fill = NA,
             color = "black",
             linetype = 3
  ) +
  ggplot2::annotate("text",
           x = 310,
           y = y_max + .35,  # just inside the box
           label = "Proprietary LLMs\n (via API)",
           hjust = 0.5,
           vjust = 1,
           fontface = "bold",
           size = 3
  ) +
  ggplot2::annotate("rect",
                    xmin = 0.9, xmax = 1200,
                    ymin = y_min3-1.6, ymax = y_max3+0.6,
                    alpha = 0.02,
                    color = "black",
                    fill = NA,
                    linetype = 2
  ) +
  ggplot2::annotate("text",
                    x = 120,
                    y = y_max3+0.35 ,  # just inside the box
                    label = "Open Weight LLMs via Ollama\n (command-r-plus via API)",
                    hjust = 0.5,
                    vjust = 1,
                    fontface = "bold",
                    size = 3
  ) +
  ggplot2::labs(
    title = "Task Completion Time by Model",
    subtitle = "<span style='color:#808080'>Cheaper</span> vs. <span style='color:#C3142D'>more expensive (time, cost) </span> LLMs by company",
    y = "",
    x = "Task Completion Time in Seconds (Log Scale)",
    caption = 'Annotated with min, median, and max times based on n=6,750 annotations per model.'
       ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = ggtext::element_markdown(hjust = 1, face = "bold", size = 10),
    axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 9),
    axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 9, face = 'bold'),
    axis.text.y = ggtext::element_markdown(hjust = 1, size = 9, face = 'bold'),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "none",
    plot.caption = ggtext::element_markdown(hjust = 0.95, size = 8),
    axis.ticks.y = ggplot2::element_blank()
  )

ggplot2::ggsave("C:\\users\\megahefm\\downloads\\time_diff_boxplot.pdf", width = 4.5, height = 6)
ggplot2::ggsave("C:\\users\\megahefm\\downloads\\time_diff_boxplot_wide.pdf", width = 6.5, height = 6.5)



