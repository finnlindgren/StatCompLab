# Code for use in project 1
# This file should not be edited.
# Author: Finn Lindgren
# Load this file into your report.Rmd document using the method shown there.

#' Approximate confidence intervals
#'
#' CI1, CI2, and CI3, are the three confidence interval constructors from
#' lab 4, for the expectation parameter in a Poisson distribution
#'
#' @param y vector of non-negative integers
#' @param alpha The nominal (desired) error probability for the
#'   confidence interval.
#'   Default: 0.05, to get 95% confidence intervals
#' @param type 1, 2, or 3, selecting one of the three parameterisation
#'   alternatives, theta=lambda, theta=sqrt(lambda), or theta=log(lambda).
#'   Default: 1
#'
#' @return A length 2 vector with the (nominally) 100*(1-alpha) %
#'   confidence interval for lambda = E(Y), where Y ~ Poisson(lambda)

pois_CI <- function(y, alpha = 0.05, type = 1) {
  if (any(y < 0)) {
    stop("All y values must be non-negative")
  }
  n <- length(y)
  if (type == 1) {
    lambda_hat <- mean(y)
    theta_interval <-
      lambda_hat - sqrt(lambda_hat / n) * qnorm(c(1 - alpha / 2, alpha / 2))
    CI <- pmax(theta_interval, 0)
  } else if (type == 2) {
    theta_hat <- sqrt(mean(y))
    theta_interval <-
      theta_hat - 1 / sqrt(4 * n) * qnorm(c(1 - alpha / 2, alpha / 2))
    CI <- pmax(theta_interval, 0)^2
  } else if (type == 3) {
    if (all(y == 0)) {
      CI <- c(0, Inf)
    } else {
      theta_hat <- log(mean(y))
      theta_interval <-
        theta_hat - 1 / sqrt(exp(theta_hat) * n) *
        qnorm(c(1 - alpha / 2, alpha / 2))
      CI <- exp(theta_interval)
    }
  } else if (type == 4) {
    # Log-likelihood approach
    theta_hat <- mean(y)
    fun_ll <- function(lambda) {
      dpois(sum(y), length(y) * lambda, log = TRUE)
    }
    ll_max <- fun_ll(theta_hat)
    fun_diff <- function(lambda) {
      fun_ll(lambda) - (ll_max - qchisq(1 - alpha, 1) / 2)
    }
    L <- uniroot(fun_diff,
                 lower = 1e-4 * max(1, theta_hat),
                 upper = max(1, theta_hat))
    U <- uniroot(fun_diff,
                 lower = theta_hat,
                 upper = 1e4 * max(1, theta_hat))
    CI <- c(L$root, U$root)
  } else {
    stop(paste0("Unknown parameterisation type ", type))
  }
  CI
}

#' Archaeological data
#'
#' @param J integer, 1, 2, 3, or 4, determining the number of excavations
#'   for which to return data.
#' @return A data.frame with columns y1 (left femurs) and y2 (right femurs)
#'   with J rows, each corresponding to a different archaeological excavation.
arch_data <- function(J) {
  df <- data.frame(
    y1 = c(256, 29, 112, 428),
    y2 = c(237, 24, 121, 407)
  )
  df[seq_len(J), , drop = FALSE]
}
