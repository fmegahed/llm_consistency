## Overview

This repository provides a comprehensive framework for assessing the consistency and validity of multiple large language models (LLMs) on binary classification and sentiment-labeling tasks. Our workflow includes:

* **Custom R functions** for making LLM API calls, extracting structured labels, and computing intra- and inter-rater reliability metrics.
* **Reproducible RMarkdown report** that documents the entire analysis, with interactive tables of contents and embedded figures.
* **Publication-ready figure scripts** to visualize agreement distributions, reliability scores, and validity comparisons.
* **Raw and processed datasets** and **result tables** for downstream analysis and replication.

## Repository Structure

```plain
.
├── all_functions.R            # Core R functions: LLM calls, label extraction, metrics computation
├── paper_figure_scripts/      # R scripts to generate figures for the manuscript
├── data/                      # Original and preprocessed datasets (CSV, RDS)
├── results/                   # Computed metrics and intermediate outputs (CSV, RDS)
├── rmarkdown/                 # Reproducible report (Rmd) and rendered HTML output
├── llm_consistency.Rproj      # RStudio project file for easy setup
├── renv.lock                  # Lockfile to restore exact package versions
├── .gitignore                 # Files and directories excluded from version control
└── README.md                  # (this file)
```

## Getting Started

### Prerequisites

* **R** (version ≥ 4.3.0)
* **RStudio** (optional, but recommended)
* **API keys** for OpenAI (and other LLM providers) stored in environment variables

### Installation

1. Clone this repository:

   ```bash
   git clone https://github.com/fmegahed/llm_consistency.git
   cd llm_consistency
   ```

2. Restore package dependencies using **renv**:

   ```r
   renv::restore()
   ```

### Configuration

Set your API keys before running any scripts:

```r
Sys.setenv(
  OPENAI_API_KEY       = "<your_openai_api_key>",
  STOCKNEWSAPI_KEY     = "<your_stocknewsapi_key>",
  OTHER_LLM_KEY        = "<optional_other_llm_key>"
)
```

## Usage

1. **Load core functions**:

   ```r
   source("all_functions.R")
   ```

2. **Inspect and preprocess data**:

   ```r
   binary_data <- read.csv("data/binary_classification_data.csv")
   stock_data  <- read.csv("data/stock_news_data.csv")
   ```

3. **Generate publication figures**:

   ```r
   source("paper_figure_scripts/labels_barplot.R")
   source("paper_figure_scripts/inter_rater.R")
   # ...other scripts as needed
   ```

4. **Render the reproducible report**:

   ```r
   rmarkdown::render("rmarkdown/llm_consistency.Rmd")
   ```

The rendered HTML report will appear as `rmarkdown/llm_consistency.html`.

## Outputs

* **results/**: Contains CSV and RDS files with computed agreement, reliability, and validity metrics.
* **rmarkdown/llm\_consistency.html**: The final interactive report.

## Contributing

Contributions, issues, and feature requests are welcome! Please feel free to open an issue or submit a pull request.

## License
This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.
