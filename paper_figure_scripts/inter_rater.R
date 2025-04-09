library(gridExtra, tidyverse)
source("all_functions.R")

binary_analysis_df <- read_csv("../results/binary_analysis_df.csv")

df_long <- binary_analysis_df %>%
  pivot_longer(cols = starts_with("rep"), names_to = "rep", values_to = "value")

df_inter <- df_long %>%
  select(chat_model, value) %>%
  group_by(chat_model) %>%
  mutate(id = row_number()) %>%
  pivot_wider(names_from = chat_model, values_from = value) %>% 
  select(-id)


df_inter <- df_inter %>% 
  rowwise() %>% 
  mutate(
    percent_agreement = mode_frequency(
      dplyr::c_across(1:14)
    ) * 100,
    percent_agreement_drop_na = mode_frequency(
      dplyr::c_across(1:14), drop_na=T
    ) * 100
    # we will need to add the other metrics here and check that our logic is correct
  )

df_inter_cheaper <- df_inter %>% 
  select("phi4-mini", "llama3.2:1B", "gemma3:1B", "deepseek-r1:1.5B", "command-r7b", 
         "gpt-4o-mini-2024-07-18", "claude-3-5-haiku-20241022")

df_inter_expensive <- df_inter %>% 
  select("phi4:latest", "llama3.2:3B", "gemma3:27B", "deepseek-r1:7B", 
         "command-r-plus-08-2024", "gpt-4o-2024-11-20", "claude-3-7-sonnet-20250219")

df_inter_cheaper$chat_model <- "Cheaper"
df_inter_expensive$chat_model <- "Expensive"

df_inter_cheaper_result <- reliability_coefs(df_inter_cheaper, 1:7)
df_inter_expensive_result <- reliability_coefs(df_inter_expensive, 1:7)

df_inter_final <- rbind.data.frame(df_inter_cheaper_result, df_inter_expensive_result)
write_csv(df_inter_final, "../results/inter_rater_reliability.csv")

round(mean(binary_analysis_df$percent_agreement/100), 3)
round(sd(binary_analysis_df$percent_agreement/100), 3)

round(mean(binary_analysis_df$percent_agreement_drop_na/100), 3)
round(sd(binary_analysis_df$percent_agreement_drop_na/100), 3)