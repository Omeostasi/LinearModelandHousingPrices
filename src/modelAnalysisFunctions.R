
library(tidyverse)
library(rlang)
library(Hmisc)
library(ggcorrplot)




#
# FUNCTION TO PLOT CORRELATION MATRIX OF NUM VARS
#

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