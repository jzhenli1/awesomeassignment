#' @title T-Learner
#' @description
#' This function predicts the CATE using the T-Learner.
#'
#' It first uses regression forest to fit model \eqn{\hat{m}(1,X)} in treated in train sample.
#'
#' Then, it uses regression forest to fit model \eqn{\hat{m}(0,X)} in controlled in train sample.
#'
#' Finally, it estimates CATE as
#'
#' \eqn{\hat{\tau}(x) = \hat{m}(1,x) - \hat{m}(0,x)}
#'
#' in test sample
#'
#' @param X_train A vector or a matrix of the covariates in training sample.
#' @param W_train A vector of the treatment indicator in training sample.
#' @param Y_train A vector of the outcomes in training sample.
#' @param X_test A vector or matrix of the covariates in test sample.
#'
#' @return A list containing a vector numeric values of CATE predictions in test sample,
#' a regression forest model fitted on the treated in training sample, and a regression
#' forest model fitted on the controlled in training sample
#'
#' @importFrom stats predict
#' @importFrom grf regression_forest
#'
#' @export
#'
#' @examples
#' tlearner(X_train, W_train, Y_train, X_test)


tlearner <- function(X_train, W_train, Y_train, X_test) {

  # Fitting model on training sample
  rfm1 <- regression_forest(X_train[W_train == 1,], Y_train[W_train == 1])
  rfm0 <- regression_forest(X_train[W_train == 0,], Y_train[W_train == 0])

  # Predicting CATE in test sample
  cate_tl <- predict(rfm1, X_test)$predictions - predict(rfm0, X_test)$predictions

  return(c(cate_tl, rfm1, rfm0))
}
