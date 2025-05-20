calculate_agreement = function(reps, ground_truth, na = 0) {
  na_rm = dplyr::if_else(na == 0, T, F, missing = F)
  
  matches = sum(reps == ground_truth, na.rm = na_rm)
  total_reps = length(reps)
  agreement = matches / total_reps
  
  return(agreement)
}


# Function to compute metrics
compute_metrics <- function(predictions, actual) {
  # Convert to factors to ensure consistency
  predictions <- factor(predictions, levels = c("Positive", "Negative"))
  actual <- factor(actual, levels = c("Positive", "Negative"))
  
  # Confusion matrix
  cm <- table(predictions, actual)
  
  # Extract values, handling cases where a category is missing
  TP <- ifelse("Positive" %in% rownames(cm) & "Positive" %in% colnames(cm), cm["Positive", "Positive"], 0)
  TN <- ifelse("Negative" %in% rownames(cm) & "Negative" %in% colnames(cm), cm["Negative", "Negative"], 0)
  FP <- ifelse("Positive" %in% rownames(cm) & "Negative" %in% colnames(cm), cm["Positive", "Negative"], 0)
  FN <- ifelse("Negative" %in% rownames(cm) & "Positive" %in% colnames(cm), cm["Negative", "Positive"], 0)
  
  # Compute metrics
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)  # True Positive Rate (Recall)
  specificity <- TN / (TN + FP)  # True Negative Rate
  precision <- TP / (TP + FP)  # Positive Predictive Value
  
  # Handle NA values in case of division by zero
  sensitivity <- ifelse(is.nan(sensitivity), NA, sensitivity)
  specificity <- ifelse(is.nan(specificity), NA, specificity)
  precision <- ifelse(is.nan(precision), NA, precision)
  
  # Compute F1 Score
  f1 <- ifelse(is.na(precision) | is.na(sensitivity) | (precision + sensitivity == 0), NA,
               2 * precision * sensitivity / (precision + sensitivity))
  
  return(c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, precision = precision, F1 = f1))
}


# Modified function to treat NAs as incorrect predictions
compute_metrics_na_penalized <- function(predictions, actual) {
  predictions[is.na(predictions)] <- "Incorrect"  # Treat NA as an incorrect classification
  actual[is.na(actual)] <- "Unknown"  # Keep actual values unchanged
  
  cm <- table(factor(predictions, levels = c("Positive", "Negative", "Incorrect")), 
              factor(actual, levels = c("Positive", "Negative", "Unknown")))
  
  TP <- cm["Positive", "Positive"]
  TN <- cm["Negative", "Negative"]
  FP <- cm["Positive", "Negative"]
  FN <- cm["Negative", "Positive"]
  FN_NA <- cm["Incorrect", "Positive"]  # Count NAs as FN
  FP_NA <- cm["Incorrect", "Negative"]  # Count NAs as FP
  
  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- TP / (TP + FN + FN_NA)
  specificity <- TN / (TN + FP + FP_NA)
  precision <- TP / (TP + FP + FP_NA)
  
  # Compute F1 Score
  f1 <- ifelse(is.na(precision) | is.na(sensitivity) | (precision + sensitivity == 0), NA,
               2 * precision * sensitivity / (precision + sensitivity))
  
  return(c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, precision = precision, F1 = f1))
}


counts_calculation <- function(df){
  # Convert to long format
  long_df <- df %>%
    gather(key = "Rater", value = "Rating", -Task)
  
  n <- length(unique(long_df))
  find_names <- sort(unique(long_df$Rating))
  
  # Count the number of each rating for each task
  all_counts <- long_df %>%
    group_by(Task, Rating) %>%
    count() %>%
    spread(key = Rating, value = n, fill = 0)
  
  # Rename columns to match the required format
  colnames(all_counts) <- c("Task", find_names)
  return(all_counts)
}


# ensemble function to ensemble the replicates
ensemble_reps = function(reps, na = 0){
  
  # handling of NAs
  na_rm = dplyr::if_else(na == 0, "no", "ifany", missing = "ifany")
  
  # get the most common value
  freq_table = table(reps, useNA = na_rm)
  max_freq = max(freq_table)
  most_common_values = names(freq_table)[freq_table == max_freq]
  
  # randomly select one of the most common values
  most_common = sample(most_common_values, 1)
  
  return(most_common)
  
}


# Function to extract the item (if present)
extract_category <- function(text, categories) {
  detected_category <- categories[str_detect(text, categories)]
  
  if (length(detected_category) == 0) {
    return(NA) # Return NA if no items found
  } else {
    return(paste(detected_category, collapse = ", ")) # Return the matched items
  }
}


extract_classification <- function(text, valid_words) {
  
  # ----------------------------
  # Helper: drop rows with unwanted words
  # ----------------------------
  
  drop_row = function(row) { 
    all(is.na(row) | row == "classification.\n\ntemp")
  }
  
  # ----------------------------
  # Helper: Pre-clean text1
  # ----------------------------
  clean_text1 <- function(text) {
    patterns <- c(
      "here ", "", 
      "classification;", "", 
      "definitive classification,", "", 
      "The classification is based on", "", 
      "classification based on", "", 
      "classification based ", "", 
      "classification requires", "it requires", 
      "classification without", "", 
      "it is impossible", "classification: Impossible", 
      "challenging classification", "", 
      "the classification challenging", "it challenging", 
      "\n---\n", " ", 
      "\\*", "", 
      '\\"', "", 
      "cannot definitively classify this news as either Positive or Negative", "", 
      "either Positive or Negative", "", 
      "as:", "as", 
      "\n</analysis>: \n<classification>:", " classification:", 
      "making it challenging to", "", 
      "classification.</classification>", "", 
      "\n\nClassification:\n-", "classification:", 
      "classification.\n\n<classification>:", "classification:", 
      "article", "news", 
      "</analysis>", "<analysis>", 
      "classification\\. ", "", 
      "classification because|classification for", "", 
      "positively", "positive", 
      "negatively", "negative", 
      "leaning slightly towards", "classification:", 
      "the impact is likely to be", "classification:"
    )
    
    for (i in seq(1, length(patterns), by = 2)) {
      text <- gsub(patterns[i], patterns[i + 1], text)
    }
    
    return(text)
  }
  
  # ----------------------------
  # Helper: Normalize sentiment pattern1 to standard format
  # ----------------------------
  normalize_sentiment_phrases1 <- function(text) {
    sentiment_patterns <- c(
      "\\n\\s*<analysis>:",
      "<classification>:(.*?)is likely to be",
      "likely be",
      "is likely to be",
      "is expected to have a",
      "Classification:(.*?)is expected to",
      "classification leans towards",
      "is likely",
      "is likely to have a",
      "leading to a",
      "likely leads to a",
      "suggest a potential",
      "Overall, the immediate"
    )
    
    sentiment_types <- "(positive|negative|neutral|mixed)"
    
    for (pattern in sentiment_patterns) {
      full_pattern <- regex(paste0(pattern, "\\s*", sentiment_types, "\\s*\\n?"), ignore_case = TRUE)
      text <- str_replace_all(
        text,
        full_pattern,
        function(m) {
          sentiment <- str_extract(m, "(?i)positive|negative|neutral|mixed")
          paste0("<classification>: ", str_to_title(sentiment), " ")
        }
      )
    }
    
    return(text)
  }
  # ----------------------------
  # Pre-clean text2
  # ----------------------------
  clean_text2 <- function(text) {
    patterns <- c(
      "\n{1,2}(<classification>)", "\\1", 
      "classification:\n\n<analysis>", "", 
      "the classification becomes challenging", "it becomes challenging", 
      "strict classification guidelines\\.", "", 
      "classification guidelines:|classification guidelines", "classification:", 
      "\n\nClassification:\nThe news has a", " classification:", 
      "classification due to", "", 
      "categorize this news as having either", "to", 
      "categorize this news as|classify this news as|classify this as|the classification should be|still classified it as|I would categorize this as|classified as|is categorized as|making it|making it a", "classification:", 
      "\n</classification>", "", 
      "I classify the impact of this news", "I would classify the impact of this news", 
      "I would classify the news", "I would classify the impact of this news"
    )
    
    for (i in seq(1, length(patterns), by = 2)) {
      text <- gsub(patterns[i], patterns[i + 1], text)
    }
    
    return(text)
  }
  
  # ----------------------------
  # Helper: Match a sentence that starts with the classification phrase and ends with a label
  # ----------------------------
  detect_sentiment_from_classification_sentence <- function(text) {
    match <- str_match(
      text,
      regex(
        "I would classify the impact.*?as\\s+[\"']?(positive|negative|mixed)[\"']?[.?!]?",
        ignore_case = TRUE
      )
    )
    
    if (!is.na(match[2])) {
      output <- paste0("<classification>: ", str_to_title(match[2]), " ")
      return(output)  # Capitalize
    } else {
      return(text)
    }
  }
  # Helper: Normalize sentiment pattern2 to standard format
  # ----------------------------
  normalize_sentiment_phrases2 <- function(text) {
    sentiment_patterns <- c(
      "I would classify this news news as", 
      "the classification is",
      "the tone of the news is",
      "the news presents a"
    )
    
    sentiment_types <- "(positive|negative|neutral|mixed)"
    
    for (pattern in sentiment_patterns) {
      full_pattern <- regex(paste0(pattern, "\\s*", sentiment_types, "\\s*\\n?"), ignore_case = TRUE)
      text <- str_replace_all(
        text,
        full_pattern,
        function(m) {
          sentiment <- str_extract(m, "(?i)positive|negative|neutral|mixed")
          paste0("<classification>: ", str_to_title(sentiment), " ")
        }
      )
    }
    
    return(text)
  }
  
  # ----------------------------
  # Helper: Detect "It is likely that..." sentences
  # ----------------------------
  detect_likely_sentiment <- function(text){
    # Extract all sentences that start with "It is likely that"
    matches <- str_match_all(
      text,
      regex("it is likely that[^.?!]*\\b(positive|negative)\\b[^.?!]*[.?!]", ignore_case = TRUE)
    )[[1]]
    
    if (nrow(matches) > 0) {
      output <- paste0("<classification>: ", str_to_title(matches[1, 2]), " ") # "Positive" or "Negative"
      return(output)
    } else {
      return(text)
    }
  }
  

  # ----------------------------
  # Step 1: Clean and Normalize
  # ----------------------------
  text <- clean_text1(text)
  text <- normalize_sentiment_phrases1(text)
  
  text <- gsub("</classification>:", "<classification>:", text)
  text_temp <- str_extract(text, "(?i)<classification>:\\s*(positive|negative|neutral|mixed)\\s*$")
  if (!is.na(text_temp)) text <- text_temp
  
  text <- clean_text2(text)
  text <- detect_sentiment_from_classification_sentence(text)
  text <- detect_likely_sentiment(text)
  text <- normalize_sentiment_phrases2(text)
  
  text = stringr::str_replace(text, "Mixed to ", "")
  text = stringr::str_replace_all(text, c(
    "Slightly" = "", # Remove the word "slightly"
    "clearly" = "",      # Remove the word "clearly"
    '\\"' = "",          # Remove escaped quotes (\\")
    '"' = ""             # Remove regular quotes (")
  ))
  
  if (str_detect(text, "<classification>: Positive")) text = "<classification>: Positive"
  if (str_detect(text, "<classification>: Negative")) text = "<classification>: Negative"
  if (str_detect(text, "<classification>: Neutral")) text = "<classification>: Neutral"
  
  # ----------------------------
  # Step 2: Extract final classification
  # ----------------------------
  # define a regex pattern to match:
  # 1. "\n\nclassification" with or without special characters or colons after it
  # 2. "\n\n<classification>:" or "\n\n<classification>word</classification>"
  matches = stringr::str_match_all(text, "(?i)(classification[^a-zA-Z]*:?\\s*\\w+|<classification>(\\w+)</classification>)")
  
  # get the last match if it exists
  if (length(matches[[1]]) > 0) {
    if (nrow(matches[[1]]) > 1) {  
      # Apply the function to filter out unwanted rows
      matches[[1]] = matches[[1]][!apply(matches[[1]], 1, drop_row), , drop = FALSE]
    }
    first_match = head(matches[[1]], 1)
    last_match = tail(matches[[1]], 1)
    word = NA
    word1 = NA
    word2 = NA
    # Handle the case for "<classification>word</classification>"
    if (!is.na(first_match[, 3])) {
      word = first_match[, 3]  # Extract word between <classification> and </classification>
    }else if (!is.na(last_match[, 3])) {
      word = last_match[, 3]
    }else {
      word1 = stringr::str_extract(first_match[, 1], "\\w+$")  # Otherwise, extract word after "classification"
      word2 = stringr::str_extract(last_match[, 1], "\\w+$")
    }
    
    # Check if the extracted word is in the valid_words list
    if (tolower(word) %in% tolower(valid_words)) {
      return(word)
    } else if (tolower(word1) %in% tolower(valid_words)){
      return(word1)
    } else if (tolower(word2) %in% tolower(valid_words)){
      return(word2)
    } else {
      return(NA)  # Return NA if no match is found
    }
  } else {
    return(NA)  # Return NA if no match is found
  }
}


intra_data_converter <- function(df){
  intra_data <- df %>%
    group_by(Task) %>% 
    group_modify(~ as_tibble(t(.x))) %>%
    ungroup()
  
  intra_data <- intra_data %>% 
    mutate(Task = 1:nrow(intra_data)) %>% 
    rename_with(~ paste0("VRater", seq_along(.)), starts_with("V"))
  return(intra_data)
}



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


# Function to get summary statistics for mean percent agreement
pa_summary = function(df, percent_variable, digits=4){
  llm_models = unique(df$chat_model)
  df_summary = data.frame(model = llm_models, mPa = rep(NA, length(llm_models)))
  
  for (i in 1:length(llm_models)){
    m1 = df |> 
      dplyr::filter(chat_model == llm_models[i]) |>
      dplyr::select(percent_variable)
    colnames(m1) <- "v1"
    
    df_summary[i,2] = paste0(
      round(mean(m1$v1/100), digits), " (", 
      round(sd(m1$v1/100), digits),   ")"
    )
  }
  return(df_summary)
}


# function to compute the other reliability metrics
# should contain one variable called chat_model,
# and vars indicates variables for raters
reliability_coefs = function(df, vars){
  no_models <- n_distinct(df$chat_model)
  df_coefs = data.frame(matrix(NA, ncol=9, nrow=5*no_models))
  
  colnames(df_coefs) = c("model", "coeff.name", "pa", "pe", "coeff.val", "coeff.se", "conf.int", "p.value", "w.name")
  
  llm_models = unique(df$chat_model)
  
  df_coefs$model = rep(llm_models, each=5)
  
  if (is.integer(vars)) vars = colnames(df)[vars]
  
  df_rates = df |>
    dplyr::select(dplyr::all_of(c("chat_model", vars)))
  
  for (i in seq(1, 5*no_models, by=5)){
    m1 = df_rates |>
      dplyr::filter(chat_model == llm_models[ceiling(i/5)]) |>
      dplyr::select(-chat_model)
    df_coefs[i,-1] = irrCAC::conger.kappa.raw(m1)$est
    df_coefs[(i+1),-1] = irrCAC::fleiss.kappa.raw(m1)$est
    df_coefs[(i+2),-1] = irrCAC::gwet.ac1.raw(m1)$est
    df_coefs[(i+3),-1] = irrCAC::bp.coeff.raw(m1)$est
    df_coefs[(i+4),-1] = irrCAC::krippen.alpha.raw(m1)$est
    
  }
  
  return(df_coefs)
  
}


sample_size_fn = function(alpha, margin_of_error, c_inverse = NULL, a = NULL, b = NULL,  fleiss = F){
  
  critical_value = qnorm(1-(alpha/2))
  
  if(fleiss == F){
    sample_size = (critical_value^2)/(c_inverse * (margin_of_error^2))
  }else{
    sample_size = ((margin_of_error / critical_value)^2 * b) / (((margin_of_error / critical_value)^2) - a)
  }
  
  sample_size = ceiling(sample_size)
  
  return(sample_size)
}

