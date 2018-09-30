#' Heteroskedastic Ridge Regression
#'
#' @author Michal Oleszak
#'
#' @param y Vector of response variable
#' @param X Matrix of predictor variables
#' @param lambda Value of ridge (L2) regularization penalty
#' @param optim_method Optimization algorithm, passed to optim()
#' @param predictor_weights Vector of length ncol(X) with weights for predictor variables.
#' Defaults to "varbased", in which case weights are based on the variance of parameter
#' estimates obtained through a set of univaraite regressions.
#'
#' @return A list with parameter estimates, fitted values and multiple R-squared.
#' @export
#' @importFrom glmnet glmnet
#'
#' @examples
hridge <- function(y, X, lambda, predictor_weights = "varbased", optim_method = "Nelder-Mead") {
  if (predictor_weights == "varbased") {
    weights <- get_varbased_weights(y, X)
  }
  model_init <- glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE)
  betas_init <- as.vector(model_init$beta)
  coef <- optim(betas_init, hridge_loss, method = optim_method)$par
  fitted <- X %*% coef
  rsq <- cor(y, fitted)^2
  names(coef) <- colnames(X)
  output <- list("coef" = coef,
                 "fitted" = fitted,
                 "rsq" = rsq)
  return(output)
}

#' Loss Function in Heteroskedastic Ridge Regression
#'
#' @author Michal Oleszak
#'
#' @param betas vector of parameter estimates
#'
#' @return A single numeric, value of the loss function.
#' @export
#'
#' @examples
hridge_loss <- function(betas) {
  sum((y - X %*% betas)^2) + lambda * sum(weights * betas^2)
}

#' Produce variance-based weights for predictors in heteroskedastic ridge regression
#'
#' @param y Vector of response variable
#' @param X Matrix of predictor variables
#'
#' @return A vector of length ncol(X) with weights for each predictor.
#' @export
#'
#' @examples
get_varbased_weights <- function(y, X) {
  weights <- sapply(seq(ncol(X)), function(predictor) {
    uni_model <- lm(y ~ X[, predictor])
    coeff_variance <- summary(uni_model)$coefficients[2, 2]^2
  })
}

