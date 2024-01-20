#' @title R-Learner
#'
#' @description
#' This function predicts the CATE using the R-Learner.
#'
#' Every supervised ML model that is capable of dealing with weighted minimization
#' problems can be applied to the R-Learner. This function uses weighted regression forests and pseudo-outcomes.
#'
#' The function first calculates the outcome and treatment residuals, then computes the pseudo-outcome by
#' dividing the outcome residual by the treatment residual. Subsequently, it creates the weights by squaring
#' the treatment residuals. Finally it fits a weighted regression tree on the covariates in the train sample and the
#' pseudo outcome and estimates CATE for the test sample.
#'
#' @param X_train A vector or a matrix of the covariates in training sample.
#' @param W_train A vector of the treatment indicator in training sample.
#' @param Y_train A vector of the outcomes in training sample.
#' @param X_test A vector or matrix of the covariates in test sample.
#' @param mhat A vector of estimates of the conditional expectation of the outcome (nuisance parameter)
#' @param ehat A vector of estimates of the conditional expectation of the treatment; estimated propensity score (nuisance parameter)
#'
#' @return A list containing a vector of numeric values of CATE predictions in test sample,
#' and a fitted weighted regression tree model.
#'
#' @importFrom stats predict
#' @importFrom grf regression_forest
#'
#' @export
#'
#' @examples
#' rlearner(X_train, W_train, Y_train, X_test, mhat, ehat)


rlearner <- function(X_train, W_train, Y_train, X_test, mhat, ehat) {
  # Create residuals
  res_y <- Y_train - mhat
  res_w <- W_train - ehat

  # Create pseudo-outcome (outcome res divided by treatment res)
  pseudo_rl <- res_y / res_w

  # Create weights
  weights_rl <- res_w^2

  # Weighted regression with random forest
  # Fitting reg forest on training sample
  rf_rl <- regression_forest(X_train, pseudo_rl, sample.weights = weights_rl)

  # Predicting CATE on test sample
  cate_rl <- predict(rf_rl, X_test)$predictions

  return(list(cate = cate_rl, rf = ref_rl))
}
