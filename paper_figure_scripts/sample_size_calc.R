
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

sample_size_fn(alpha = (1-.9^(1/7)), margin_of_error = 0.1, c_inverse = 11)

sample_size_fn(alpha = 0.1, margin_of_error = 0.05, a = 0.077, b = 1.93, fleiss = T)

Cp_values = readr::read_rds("Cp_values.RDS")

sample_size_fn(alpha = (1-.9^(1/7)), margin_of_error = 0.06, 
               c_inverse = Cp_values$BP |> 
                 dplyr::filter(Categories == 2, Raters == 5) |> 
                 dplyr::pull(Inv_C))

sample_size_fn(alpha = (1-.9^(1/7)), margin_of_error = 0.05, 
               c_inverse = Cp_values$Pa |> 
                 dplyr::filter(Categories == 2, Raters == 5) |> 
                 dplyr::pull(Inv_C))



# * Plot ----------------------------------------------------------
my_red = '#C3142D'
my_gray = '#808080'

Cp_values = Cp_values |>
  purrr::set_names(c("AC1", "BP", "PA"))

# Prepare parameters
methods = names(Cp_values)
rater_range = seq(5,9,2)
margin_of_error = 0.05
category_val = 2


# Compute sample size for each method and rater count
plot_data = purrr::map_dfr(methods, function(method) {
  df = Cp_values[[method]]
  
  purrr::map_dfr(rater_range, function(r) {
    alpha = 1 - 0.9^(1 / 7)
    
    c_inv = dplyr::filter(df, Categories == category_val, Raters == r) |>
      dplyr::pull(Inv_C)
    
    if (base::length(c_inv) == 0 || base::is.na(c_inv)) return(NULL)
    
    tibble::tibble(
      Method = method,
      Raters = r,
      SampleSize = sample_size_fn(
        alpha = alpha,
        margin_of_error = margin_of_error,
        c_inverse = c_inv
      )
    )
  })
})

# Create individual ggplot2 plots for each method
library(magrittr)

max_sample_size = max(plot_data$SampleSize)


plots = plot_data %>%
  base::split(.$Method) %>%
  purrr::map(
    ~ggplot2::ggplot(.x, ggplot2::aes(x = Raters, y = SampleSize)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_text(
        ggplot2::aes(
          label = scales::comma(SampleSize),
          color = factor(ifelse(SampleSize == max_sample_size, my_red, my_gray),
                         levels = c(my_gray, my_red)),
          fontface = 'bold'
          # fontface = ifelse(SampleSize == max_sample_size, "bold", "plain")
          ),
        vjust = -1,
        size = 3,
      ) +
      ggplot2::scale_color_manual(values = c(my_gray, my_red)) +
      ggplot2::scale_x_continuous(breaks = rater_range, lim=c(4,10)) +
      ggplot2::scale_y_continuous(lim = c(0, 1800), labels = scales::comma, breaks = scales::pretty_breaks(5)) +
      ggplot2::labs(
        title = .x$Method[1],
        x = "Number of Raters",
        y = "Sample Size"
        ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 11),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 10),
        axis.title.y = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 10),
        axis.text.x = ggtext::element_markdown(hjust = 0.5, size = 9),
        axis.text.y = ggtext::element_markdown(hjust = 0.5, size = 9),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "none",
        plot.caption = ggtext::element_markdown(hjust = 0.5, size = 8)
      )
    )


# Combine the plots side-by-side
patchwork::wrap_plots(
  plots,
  ncol = 3,
  guides = "collect",
  tag_level = 'new',
  axis_titles = 'collect'
)

ggplot2::ggsave(
  "sample_size_plot.png", 
  width = 6.5, height = 2, dpi = 600, units = "in",
)
