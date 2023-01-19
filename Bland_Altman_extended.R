## Bland-Altman extended

# Buchan DS & McLellan G. Comparing physical activity estimates in children from hip-worn Actigraph GT3X+ accelerometers
# using raw and counts based processing methods. J Sport Sci 2019;37(7):779-787


# Helper function
# Calculate percentage
BA_calculate_percentage <- function(lw_limit,
                                    up_limit,
                                    diff_methods){

  # Number of diff points outside limits of agreement
  n_outsiders <- c()
  for (i in 1:length(diff_methods)) {

    if(diff_methods[i] > up_limit | diff_methods[i] < lw_limit) {
      n_outsiders[i] <- 1
    } else {
      n_outsiders[i] <- 0
    }
  }

  # Percentage
  n_outside <- sum(n_outsiders)
  n_total <- length(n_outsiders)
  BA_percentage <- round(n_outside / n_total, 2)

  return(list(BA_percentage = BA_percentage,
              n_outside = n_outside,
              n_total = n_total))
} # end BA_calculate_percentage


# Main function
Bland_Altman_extended <- function(method1,
                                  method2,
                                  x_lab = NULL,
                                  y_lab = NULL,
                                  save_plot = T,
                                  plot_name = NULL) {

  ## Check libraries
  # Tidyverse
  if (!require(tidyverse)) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  # BlandAltmanLeh
  if (!require(BlandAltmanLeh)) {
    install.packages("BlandAltmanLeh")
    library(BlandAltmanLeh)
  }

  # Percentage of Diff points outside limits of agreement
  BA_percentage <- BA_calculate_percentage(lw_limit = bland.altman.stats(method1, method2)$lower.limit,
                                           up_limit = bland.altman.stats(method1, method2)$upper.limit,
                                           diff_methods = bland.altman.stats(method1, method2)$diffs)


  # Compute one-sample t-test
  p_value_t_test <- t.test(bland.altman.stats(method1, method2)$diffs,
                           mu = 0)$p.value

  sig <- c()
  if(p_value_t_test < 0.001) {
    sig <- "***"
  } else if(p_value_t_test < 0.01){
    sig <- "**"
  } else if(p_value_t_test < 0.05){
    sig <- "*"
  } else{
    sig <- ""
  }

  # Percentage of points outside 95% LoA
  percentage_title <- paste("(",
                            BA_percentage$n_outside,
                            "/",
                            BA_percentage$n_total,
                            ")",
                            " outside the limits of agreement",
                            sep = "")



  # Bias difference title
  differences_title <- paste("mean differences = ",
                             round(bland.altman.stats(method1, method2)$mean.diffs, 2),
                             sig,
                             sep = "")

  # 95 limits of agreement title
  LoA_title <- paste("95% limits of Agreement = (",
                     round(bland.altman.stats(method1, method2)$lower.limit, 2),
                     ", ",
                     round(bland.altman.stats(method1, method2)$upper.limit, 2),
                     ")",
                     sep = "")


  # Main plot
  BA_plot <- bland.altman.plot(method1, method2, graph.sys = "ggplot2")

  # Custom plot
  BA_plot <- BA_plot +
    geom_point(size = 4) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=12),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste(percentage_title, "\n",
                  differences_title, "\n",
                  LoA_title))

  if(!is.null(x_lab)) {
    BA_plot <- BA_plot + xlab(x_lab)
  }

  if(!is.null(y_lab)) {
    BA_plot <- BA_plot + ylab(y_lab)
  }

  if(save_plot == T & !is.null(plot_name)){
    ggsave(paste(plot_name,".png", sep = ""), height=5, width=7, units='in', dpi=600)
  }

  return(BA_plot)
} # end BA extended
