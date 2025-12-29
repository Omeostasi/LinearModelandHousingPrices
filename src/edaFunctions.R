# Functions that are going to be used by EDA rmd
library(tidyverse)
library(rlang)
library(Hmisc)
library(ggcorrplot)
#
# TO ANALYZE NUMERICAL VARIABLES
#
numerical_descriptive <- function(data) {
  
  # Keep only numeric columns
  num_data <- dplyr::select_if(data, is.numeric)
  
  # Compute summary statistics for each variable
  stats <- num_data %>%
    summarise(across(
      everything(),
      list(
        n_obs = ~round(sum(!is.na(.)), 0),
        n_miss = ~round(sum(is.na(.)), 0),
        mean = ~round(mean(., na.rm = TRUE), 3),
        sd = ~round(sd(., na.rm = TRUE), 3),
        min = ~min(., na.rm = TRUE),
        q25 = ~quantile(., 0.25, na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        q75 = ~quantile(., 0.75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        skewness = ~round(moments::skewness(., na.rm = TRUE), 3),
        kurtosis = ~round(moments::kurtosis(., na.rm = TRUE),3)
      ),
      .names = "{.fn}_{.col}"
    ))
  
  # Reshape: statistics in rows, variables in columns
  stats_long <- stats %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = c("statistic", "variable"),
                        names_sep = "_(?=[^_]+$)") %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  # Arrange rows logically
  stat_order <- c("n_obs", "n_miss", "mean", "sd", "min", "q25", "median", "q75", "max", "skewness", "kurtosis")
  stats_long <- stats_long %>%
    dplyr::mutate(statistic = factor(statistic, levels = stat_order)) %>%
    dplyr::arrange(statistic)
  
  # Return a clean data frame
  return(as.data.frame(stats_long, 4))
}

#
# TO ANALYZE DISCRETE VARIABLES
#
categorical_summary <- function(data, digits = 3) {
  
  # Select only categorical variables
  cat_data <- dplyr::select_if(data, ~is.factor(.) | is.character(.))
  if (ncol(cat_data) == 0) stop("No categorical variables found in the dataset.")
  
  results <- lapply(names(cat_data), function(var) {
    x <- cat_data[[var]]
    n_obs <- sum(!is.na(x))
    n_miss <- sum(is.na(x))
    n_levels <- length(unique(na.omit(x)))
    
    freq <- sort(table(x, useNA = "no"), decreasing = TRUE)
    mode_level <- names(freq)[1]
    mode_count <- as.numeric(freq[1])
    mode_percent <- round(mode_count / n_obs, digits)
    
    tibble(
      statistic = c("n_obs", "n_miss", "n_levels", "mode", "mode_freq", "mode_percent"),
      !!var := c(
        n_obs,
        n_miss,
        n_levels,
        mode_level,
        mode_count,
        mode_percent
      )
    )
  })
  
  # Combine by full join on "statistic"
  result <- Reduce(function(x, y) full_join(x, y, by = "statistic"), results)
  
  # Order statistics
  stat_order <- c("n_obs", "n_miss", "n_levels", "mode", "mode_freq", "mode_percent")
  result <- result %>%
    mutate(statistic = factor(statistic, levels = stat_order)) %>%
    arrange(statistic)
  
  return(as.data.frame(result))
}


# DATA VISUALIZATION - HISTOGRAM / DISTRIBUTIONS

data_visualization <- function(data, bins = 30){
  numeric <- select(data, where(~is.numeric(.)))
  numericColsNames <- colnames(numeric)
  
  plots <- c()
  
  for (numName in numericColsNames){
    
    plot <- ggplot(
      data = data,
      aes(x= .data[[numName]])
    )    +
      geom_histogram(aes(y = ..density..), bins = bins, fill = "steelblue", color = "white", alpha = 0.7) +
      geom_density(color = "darkred", size = 1) +
      labs(
        title = paste("Histogram of", numName),
        x = numName,
        y = "Density"
      ) +
      theme_minimal()
    
    plots <- c(plots, plot)
  }
  
  
  
  categorical <- select(data, where(~is.factor(.) | is.character(.)))
  categoricalNames <- colnames(categorical)
  
  for (catName in categoricalNames){
    plot <- ggplot(
      data = data,
      aes( x = .data[[catName]])
    ) +
      geom_bar(fill = "steelblue", alpha = 0.8) +
      labs(
        title = paste("Barplot of", catName),
        x = catName,
        y = "Count"
      ) +
      theme_minimal()
    plots <- c(plots, plot)
  }
  
  return(plots)
}


# PAIRWISE LINEARITY CHECK PLOTS

pairwiseplot_for_linearity <- function(data, y_var) {
  
  if (!y_var %in% names(data)) stop("y_var not found in dataset.")
  if (!is.numeric(data[[y_var]])) stop("y_var must be numeric.")
  
  plots <- list()
  
  for (x_var in setdiff(names(data), y_var)) {
    x <- data[[x_var]]
    
    # Numerical x: scatter plot + regression line
    if (is.numeric(x)) {
      p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", color = "darkred", se = FALSE) +
        labs(title = paste(y_var, "vs", x_var), x = x_var, y = y_var) +
        theme_minimal()
      
      # Categorical x: boxplot + mean points
    } else if (is.factor(x) || is.character(x)) {
      p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
        geom_boxplot(fill = "steelblue", alpha = 0.6) +
        stat_summary(fun = mean, geom = "point", color = "darkred", size = 3) +
        labs(title = paste(y_var, "by", x_var), x = x_var, y = y_var) +
        theme_minimal() 
    } else {
      next  # skip unsupported types
    }
    
    plots[[x_var]] <- p
  }
  
  return(plots)
}


# CORRELATION MATRIX

plot_correlation_matrix <- function(data, method = "pearson", show_pvalues = TRUE,
                                    type = "lower", label = TRUE,
                                    palette = c("#6D9EC1", "white", "#E46726"),
                                    title = "Correlation Matrix") {
  # Required packages
  # Keep only numeric variables
  numeric_data <- data[sapply(data, is.numeric)]
  
  if (ncol(numeric_data) < 2) {
    stop("There must be at least two numeric variables to compute correlations.")
  }
  
  # Compute correlation matrix
  corr <- cor(numeric_data, use = "pairwise.complete.obs", method = method)
  
  # Optional: compute p-values
  if (show_pvalues) {
    res <- Hmisc::rcorr(as.matrix(numeric_data))
    pmat <- res$P
    ggcorrplot(corr, p.mat = pmat, type = type, lab = label,
               colors = palette, title = title, ggtheme = ggplot2::theme_bw())
  } else {
    ggcorrplot(corr, type = type, lab = label,
               colors = palette, title = title, ggtheme = ggplot2::theme_bw())
  }
}
