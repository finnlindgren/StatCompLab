#' @title Calculate proper scores
#' @description Calculates proper scores for probabilistic predictions
#' @param type A string indicating the type of score to calculate. One of
#' "se", "ae", "ds", or "interval". See Details below.
#' @param obs A vector of observations
#' @param ... Additional named arguments needed for each type of score, see
#' Details below.
#' @return A vector of calculated scores, one for each observation
#' @details
#' Each score type takes additional arguments that define the prediction model
#' information to be used in the score calculations. Unless specified otherwise,
#' the extra arguments should either be scalars (applying to all the predictions)
#' or vectors of the same length as the `obs` vector.
#' \describe{
#' \item{se}{Squared error score. Requires `mean`, defining the prediction mean}
#' \item{ae}{Absolute error score. Requires `median`, defining the prediction median}
#' \item{ds}{Dawid-Sebastiani score. Requires `mean` and `sd`, defining the mean and
#' standard deviation of the predictions}
#' \item{interval}{Squared error score. Requires `lwr` and `upr`, defining the
#'   lower and upper prediction interval endpoints, and `alpha`, defining the
#'   prediction error probability targeted by the prediction intervals}
#' }
#' The scores \eqn{S_{type}(F_i,y_i)}{S_type(F_i,y_i)} are defined as in the lecture notes,
#' with `obs[i]` equal to \eqn{y_i} and each prediction distribution
#' \eqn{F_i} defined by additional named arguments.
#'
#' @examples
#' # Five realisations of N(3, 3^2)
#' obs <- rnorm(5, mean = 3, sd = 3)
#'
#' # One prediction for each observation. Only one of the predictions describes
#' # the true model
#' F_mean <- 1:5
#' F_median <- 1:5
#' F_sd <- 1:5
#' F_lwr <- F_mean - F_sd * qnorm(0.9)
#' F_upr <- F_mean - F_sd * qnorm(0.1)
#'
#' # Compute the scores
#' data.frame(
#'   se = proper_score("se", obs, mean = F_mean),
#'   ae = proper_score("ae", obs, median = F_median),
#'   ds = proper_score("ds", obs, mean = F_mean, sd = F_sd),
#'   interval = proper_score("interval", obs,
#'                           lwr = F_lwr, upr = F_upr, alpha = 0.2)
#' )
#' @export
#' @rdname proper_score

proper_score <- function(type, obs, ...) {
  type <- match.arg(type, c("se", "ae", "ds", "interval"))
  required <- list(
    se = c("mean"),
    ae = c("median"),
    ds = c("mean", "sd"),
    interval = c("lwr", "upr", "alpha")
  )
  args <- list(...)
  if (length(setdiff(required[[type]], names(args))) > 0) {
    miss <- setdiff(required[[type]], names(args))
    stop(paste0("Missing arguments '",
                paste0(miss, collapse = "', '"),
                "' for score type='", type, "'"))
  }
  if (length(setdiff(names(args), required[[type]])) > 0) {
    extra <- setdiff(names(args), required[[type]])
    warning(paste0("Extra arguments '",
                paste0(extra, collapse = "', '"),
                "' ignored for score type='", type, "'"))
  }
  if (identical(type, "se")) {
    (obs - args$mean)^2
  } else if (identical(type, "ae")) {
    abs(obs - args$median)
  } else if (identical(type, "ds")) {
    (obs - args$mean)^2/args$sd^2 + 2 * log(args$sd)
  } else if (identical(type, "interval")) {
    stopifnot(args$alpha > 0)
    stopifnot(args$alpha < 1)
    args$upr - args$lwr + 2 / args$alpha * (
      (args$lwr - obs) * (obs < args$lwr) +
      (obs - args$upr) * (obs > args$upr)
    )
  } else {
    stop("Internal error")
  }
}












# model_Z: Compute model matrices for use by calc_EV

filament1_model_Z <- function(formulas, data) {
  ZE <- model.matrix(formulas$E, data = data)
  ZV <- model.matrix(formulas$V, data = data)
  list(ZE = cbind(ZE, ZV * 0), ZV = cbind(ZE * 0, ZV))
}

# calc_EV: Evaluate the expectation and variance for model A and B
# Input:
#   theta : A vector with theta1 and theta2
#   data : A data.frame containing the required predictors, including CAD_Weight
#   model : A string denoting which model to use, either "A" or "B".
#   Sigma_theta : If not NULL, an estimate of the covariance matrix for
#                 the uncertainty of estimated theta1 and theta2
# Output:
#   A list with four elements:
#     E : E(y|theta,x)
#     V : Var(y|theta,x)
#     VE : Var(E(y|theta,x)|x) or NULL
#     EV : E(Var(y|theta,x)|x) or NULL

filament1_calc_EV <- function(theta, data, model = c("A", "B"),
                    Sigma_theta = NULL) {
  model <- match.arg(model)
  if (model == "A") {
    Z <- filament1_model_Z(
      list(
        E = ~ 1 + CAD_Weight,
        V = ~ 1 + CAD_Weight
      ),
      data
    )
    VE <- EV <- NULL
    if (!is.null(Sigma_theta)) {
      # E(Var(y|theta,x)|x)
      EV <- exp(Z$ZV %*% theta + rowSums(Z$ZV * (Z$ZV %*% Sigma_theta)) / 2)
      # Var(E(y|theta,x)|x)
      VE <- rowSums(Z$ZE * (Z$ZE %*% Sigma_theta))
    }
    out <- list(
      E = Z$ZE %*% theta,
      V = exp(Z$ZV %*% theta),
      VE = VE,
      EV = EV
    )
  } else {
    Z <- filament1_model_Z(
      list(
        E = ~ 1 + CAD_Weight,
        V = ~ 1 + I(CAD_Weight^2)
      ),
      data
    )
    VE <- EV <- NULL
    if (!is.null(Sigma_theta)) {
      # E(Var(y|theta,x)|x)
      # (pmin: Ignore large Sigma_theta values)
      EV <- Z$ZV %*% exp(theta + pmin(0.5^2, diag(Sigma_theta)) / 2)
      # Var(E(y|theta,x)|x)
      VE <- rowSums(Z$ZE * (Z$ZE %*% Sigma_theta))
    }
    out <- list(
      E = Z$ZE %*% theta,
      V = Z$ZV %*% exp(theta),
      VE = VE,
      EV = EV
    )
  }
  out
}



# neg_log_like: Evaluate the negated log-likelihood for model A and B
# Input:
#   theta : A vector with theta1 and theta2
#   data : A data.frame containing the required predictors, including CAD_Weight
#   model : A string denoting which model to use, either "A" or "B".

filament1_neg_log_like <- function(theta, data, model = c("A", "B")) {
  model <- match.arg(model)
  EV <- filament1_calc_EV(theta, data, model)
  - sum(dnorm(data[["Actual_Weight"]],
              mean = EV$E,
              sd = EV$V^0.5,
              log = TRUE
  ))
}



# estimate_and_predict: Estimate model and compute predictions
#
# Input:
#   data : A data.frame with data used for parameter estimation
#   newdata : A data.frame with data used for prediction
#   model : A string determining which model to use, either "A" or "B"
#   alpha: The target error probability for the prediction intervals
#   df: Degrees of freedom for t-quantiles for the interval construction.
#       Inf = Normal distribution.
# Output:
#   A data.frame with columns 'mu', 'sigma', 'lwr', 'upr', also see
#   estimate_and_predict_output_template

#' @title Estimate filament models
#' @description Estimate filament models with different variance structure
#' @param data A `data.frame` with the same variables as the `filament1` data set.
#' Must have columns `CAD_Weight` and `Actual_Weight`
#' @param model Either "A" for a log-linear variance model, or "B" for a proportional
#' scaling error model
#' @return An estimation object suitable for use with [filament1_predict()]
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname filament1_estimate

filament1_estimate <- function(data, model) {
  model <- match.arg(model, c("A", "B"))
  if (model == "A") {
    theta_start <- c(-0.1, 1.07, -2, 0.05)
  } else {
    theta_start <- c(-0.15, 1.07, -13.5, -6.5)
  }
  opt <- optim(theta_start,
               filament1_neg_log_like,
               data = data,
               model = model,
               hessian = TRUE,
               method = "Nelder-Mead",
               control = list(maxit = 5000)
  )
  fit <- list(
    model = model,
    theta = opt$par,
    hessian = opt$hessian
  )
  class(fit) <- c("filament1_estimate", "list")
  fit
}



#' @title Predict values for a filament model
#' @description Uses an estimated filament model to predict new observations
#' @param object The estimation object, like the output from [filament1_estimate()]
#' @param newdata The data to use for prediction.
#' Must have a column `CAD_Weight`
#' @param alpha The target coverage error for the prediction intervals, Default: 0.05
#' @param df The degrees of freedom for t-quantiles for the prediction interval
#' construction, Default: Inf
#' @param ... Additional arguments, currently ignored
#' @return A data.frame with variables `mean`, `sd`, `lwr`, and `upr`, summarising
#' the prediction distribution for each row of `newdata`.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname filament1_predict

filament1_predict <- function(object, newdata, alpha = 0.05, df = Inf, ...) {
  stopifnot(inherits(object, "filament1_estimate"))
  model <- match.arg(object$model, c("A", "B"))

  pred_EV <- filament1_calc_EV(
    object$theta,
    data = newdata,
    model = model,
    Sigma_theta = solve(object$hessian)
  )
  pred_sd <- (pred_EV$EV + pred_EV$VE)^0.5

  q <- qt(1 - alpha / 2, df = df)
  lwr <- pred_EV$E - q * pred_sd
  upr <- pred_EV$E + q * pred_sd

  data.frame(mean = pred_EV$E, sd = pred_sd,
             lwr = lwr, upr = upr)
}
