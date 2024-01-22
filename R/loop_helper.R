#' @title Helper function for Loops
#'
#' @description
#' Helper function to run a loop over several seeds to explore the variability of CATE estimates.
#'
#' @param seeds A list of seeds to loop over
#'
#' @return A grid of plots
#'
#' @importFrom stats predict
#' @importFrom grf regression_forest
#' @importFrom grf causal_forest
#' @importFrom cowplot plot_grid
#' @import ggplot2
#' @import evaluCATE
#'
#'
#' @export
#'
#' @examples
#' loop_helper(seeds = c(1, 2, 3))

loop_helper <- function(seeds) {

  # Initialize empty lists to store plots and matrices
  hist_plots <- list()
  plot_cfvar_plots <- list()
  cf_gatevar_plots <- list()
  combined_pvals_list <- list()

  # Looping over seeds
  for (i in 1:length(seeds)) {
    # Setting seed
    set.seed(seeds[i])

    # Fitting model in training sample
    cfvar <- causal_forest(X_train, Y_train, W_train)

    # Predicting CATE in test sample
    cate_cfvar <- predict(cfvar, X_test)$predictions

    # Visualization
    hist <- ggplot(data = as.data.frame(cate_cfvar), aes(x = cate_cfvar)) +
      geom_histogram(binwidth = 0.01, fill = "grey", color = "black") +
      geom_vline(xintercept = mean(cate_cfvar), color = "red", linewidth = 1) +
      geom_text(aes(x = mean(cate_cfvar), y = 1000, label = paste("Mean CATE:", round(mean(cate_cfvar), 6))),
                vjust = 2, hjust = 1.2, color = "red", size = 2.5) +
      labs(title = paste0("Seed=", i, ", Split 30:70"), x = "CATE", y = "Frequency")

    hist_plots[[i]] <- hist

    # Derive logical vector train_idx
    train_idx <- rep(FALSE, length(Y))
    train_idx[indices] <- TRUE

    # Full sample prediction
    cate_cf_evalvar <- predict(cfvar, X)$predictions


    ## Call main function
    evaluation_cfvar <- evaluCATE(Y, W, X, cate_cf_evalvar, train_idx,
                                  pscore = propensity_score, verbose = FALSE, n_groups = 4)

    # Using helper function from my package to plot BLP
    plot_cfvar <- awesomeassignment::BLP_plot(evaluation_cfvar,
                                              paste0("Seed=", seeds[i], ", Split 30:70"))
    plot_cfvar_plots[[i]] <- plot_cfvar

    cf_gatevar <- plot(evaluation_cfvar, target = "GATES") +
      ggtitle(paste0("Seed=", i, ", Split 30:70"))

    cf_gatevar_plots[[i]] <- cf_gatevar

    cf_pval <- pvals(evaluation_cfvar, paste0("Seed=", seeds[i]))
    combined_pvals_list[[i]] <- cf_pval
  }

  # Arrange the plots in a grid layout
  hist_grid <- plot_grid(plotlist = hist_plots, ncol = 2, nrow = 2)
  plot_cfvar_grid <- plot_grid(plotlist = plot_cfvar_plots, ncol = 2, nrow = 2)
  cf_gatevar_grid <- plot_grid(plotlist = cf_gatevar_plots, ncol = 2, nrow = 2)

  # Combine the p-values into a matrix
  pvals_matrix <- do.call(cbind, combined_pvals_list)

  # Return the arranged grids
  return(list(hist_grid, plot_cfvar_grid, cf_gatevar_grid, pvals_matrix))
}
