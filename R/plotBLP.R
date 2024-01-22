#' @title Plot the \eqn{\beta_2} estimates of the Best Linear Predictor
#'
#' @param model An `evaluCATE` object
#' @param name A string of the plot title
#'
#' @return A plot of the \eqn{\beta_2} estimates of BLP
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' BLP_plot(evaluation_tl, "T-Learner RF")

BLP_plot <- function(model, name) {

  # Define the row names
  rownames <- c("wr_none", "wr_cddf1", "wr_cddf2", "wr_mck1", "ht_none",
                "ht_cddf1", "ht_cddf2", "ht_mck1", "ht_mck2", "ht_mck3", "aipw")

  # Initialize a matrix to store the results
  results <- matrix(NA, nrow = length(rownames), ncol = 3)
  rownames(results) <- rownames
  colnames(results) <- c("beta2", "CILower", "CIUpper")

  # Iterate through each model component and extract beta2 and CI
  for (i in rownames) {
    beta2_estimate <- model[["BLP"]][[i]][["coefficients"]][["beta2"]]
    ci_lower <- model[["BLP"]][[i]][["conf.low"]][["beta2"]]
    ci_upper <- model[["BLP"]][[i]][["conf.high"]][["beta2"]]
    results[i, ] <- c(beta2_estimate, ci_lower, ci_upper)
  }

  results_df = as.data.frame(results)
  results_df$rowname = rownames

  # Plot coefficients and confidence intervals
  ggplot(results_df, aes(x = rowname, y = beta2, ymin = CILower, ymax = CIUpper)) +
    geom_pointrange() +
    xlab("Coefficient Name") +
    ylab("Coefficient Value") +
    ggtitle(name) +
    geom_hline(yintercept = c(0,1), linetype = c("solid","dashed")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}
