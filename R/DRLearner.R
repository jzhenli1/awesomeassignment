#' @title DR-Learner
#'
#' @description
#' This function predicts the CATE using the DR-Learner.
#'
#' The DR-Learner uses the pseudo-outcome and applies it in a generic Machine Learning setting.
#' This specific function applies the DR-Learner in a regression forest setting.
#'
#' \eqn{\hat{\tau}^{drl}(X) = \underset{\tau}{\operatorname{argmin}} \sum_{i=1}^{N} \bigg(\tilde{Y}_{i, ATE} - \tau(X_i) \bigg)^2}
#'
#' As a first step, the function runs a regression forest model on the covariates and pseudo-outcomes
#' in the training sample. Subsequently, it predicts CATEs in test sample.
#'
#' @param X_train A vector or a matrix of the covariates in training sample.
#' @param Y_tilde A vector of the pseudo-outcomes in training sample.
#' @param X_test A vector or matrix of the covariates in test sample.
#'
#' @return A vector of numeric values of CATE predictions in test sample.
#'
#' @importFrom stats predict
#' @importFrom grf regression_forest
#'
#' @export
#'
#' @examples
#' drlearner(X_train, Y_tilde, X_test)


drlearner <- function(X_train, Y_tilde, X_test) {

  # Fitting reg forest on training sample
  rf_drl <- regression_forest(X_train, Y_tilde)

  # Predicting CATE in test sample
  cate_drl <- predict(rf_drl, X_test)$predictions

  return(cate_drl)
}
