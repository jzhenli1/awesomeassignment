#' @title Extract p-values
#'
#' @description
#' Extract p-values from the GATE results of the `evaluCATE` package.
#'
#' The p-values extracted are specifically of the hypothesis test \eqn{H_0: \ \gamma_k = \gamma_1}
#'
#' @param model An `evaluCATE` object/model considered
#' @param col_name A string object of how the column of p-values should be called
#' @param rownames A list of row names to be plotted (from the evaluCATE package)
#'
#' @return A vector of p-values
#' @export
#'
#' @examples
<<<<<<< HEAD
#' pvals(evaluation_tl, "tl_pval")
=======
#' pvals(evaluation_tl, "tl_pval", rownames)
>>>>>>> c941d24b6339ec01b8d1031e9062fb9973d0a29f

pvals <- function(model, col_name, rownames=rownames) {
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
