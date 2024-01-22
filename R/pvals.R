#' @title Extract p-values
#'
#' @description
#' Extract p-values from the GATE results of the `evaluCATE` package.
#'
#' The p-values extracted are specifically of the hypothesis test \eqn{H_0: \ \gamma_k = \gamma_1}
#'
#' @param model An `evaluCATE` object/model considered
#' @param col_name A string object of how the column of p-values should be called
#'
#' @return A vector of p-values
#' @export
#'
#' @examples
#' pvals(evaluation_tl, "tl_pval")

pvals <- function(model, col_name) {

  # Define the row names
  rownames <- c("wr_none", "wr_cddf1", "wr_cddf2", "wr_mck1", "ht_none",
                "ht_cddf1", "ht_cddf2", "ht_mck1", "ht_mck2", "ht_mck3", "aipw")

  # Initialize a matrix to store the results
  pval_mat <- matrix(NA, nrow = length(rownames), ncol = 1)
  rownames(pval_mat) <- rownames
  colnames(pval_mat) <- col_name

  # Iterate through each model component and extract beta2 and CI
  for (i in rownames) {
    pval <- model[["GATES"]][[i]][["p.value.gates.largest.difference"]]
    pval_mat[i, ] <- pval
  }
  return(pval_mat)
}
