# Finn Lindgren (s0000000, finnlindgren)

# Part 1 ####

# Simulate samples of size n from Pois(lambda), m times, and construct
# confidence intervals using one of the three methods implemented in pois_CI
#
# Input:
#   m: The number of samples
#   n: The size of each sample
#   lambda: The True value of the lambda-parameter
#   alpha: The target/nominal error probability for the confidence intervals
#   type: 1, 2, 3, or 4, indicating which CI method to use
#
# Output:
#   A data frame with `m` rows and columns `Lower` and `Upper`, with one
#   confidence interval per row

multi_pois_CI <- function(m, n, lambda, alpha, type) {
  Lower = numeric(m)
  Upper = numeric(m)
  for (m_idx in seq_len(m)) {
    y <- rpois(n, lambda = lambda)
    CI <- pois_CI(y = y, alpha = alpha, type = type)
    Lower[m_idx] <- CI[1]
    Upper[m_idx] <- CI[2]
  }
  result <- data.frame(Lower = Lower, Upper = Upper)
}

tidy_multi_pois_CI_alt <- function(m, n, lambda, alpha) {
  Lower <- matrix(0, m, 4)
  Upper <- matrix(0, m, 4)
  for (m_idx in seq_len(m)) {
    y <- rpois(n, lambda = lambda)
    for (type in 1:4) {
      CI <- pois_CI(y = y, alpha = alpha, type = type)
      Lower[m_idx, type] <- CI[1]
      Upper[m_idx, type] <- CI[2]
    }
  }
  data.frame(
    Index = rep(1:m, times = 4),
    Type = rep(1:4, each = m),
    Lower = as.vector(Lower),
    Upper = as.vector(Upper)
  )
}

# Simulate samples of size n from Pois(lambda), m times, and construct
# confidence intervals using all three methods implemented in pois_CI.
# This implementation uses independent simulations for the three method, by
# calling multi_pois_CI for each interval type. An alternative implementation
# would be to use the same samples for all three methods.
#
# Input:
#   m: The number of samples
#   n: The size of each sample
#   lambda: The True value of the lambda-parameter
#   alpha: The target/nominal error probability for the confidence intervals
#
# Output:
#   A data frame with `3m` rows and columns `Lower`, `Upper`, and `Type`,
#   with one confidence interval per row, calculated with the method indicated
#   in the `Type` column

tidy_multi_pois_CI <- function(m, n, lambda, alpha) {
  result <-
    rbind(
      cbind(
        multi_pois_CI(
          m = m, n = n, lambda = lambda,
          alpha = alpha, type = 1
        ),
        Type = 1
      ),
      cbind(
        multi_pois_CI(
          m = m, n = n, lambda = lambda,
          alpha = alpha, type = 2
        ),
        Type = 2
      ),
      cbind(
        multi_pois_CI(
          m = m, n = n, lambda = lambda,
          alpha = alpha, type = 3
        ),
        Type = 3
      ),
      cbind(
        multi_pois_CI(
          m = m, n = n, lambda = lambda,
          alpha = alpha, type = 4
        ),
        Type = 4
      )
    )
  result
}


# Part 2 ####

# Joint log-probability
#
# Usage:
#   log_prob_NY(N, Y, xi, a, b)
#
# Calculates the joint probability for N and Y, when
#   N_j ~ Geom(xi)
#   phi ~ Beta(a, b)
#   Y_{ji} ~ Binom(N_j, phi)
#   For j = 1,...,J
#
# Inputs:
#   N: vector of length J
#   Y: data.frame or matrix of dimension Jx2
#   xi: The probability parameter for the N_j priors
#   a, b: the parameters for the phi-prior
#
# Output:
#   The log of the joint probability for N and Y
#   For impossible N,Y combinations, returns -Inf

log_prob_NY <- function(N, Y, xi, a, b) {
  # Convert Y to a matrix to allow more vectorised calculations:
  Y <- as.matrix(Y)
  if (any(N < Y) || any(Y < 0)) {
    return(-Inf)
  }
  atilde <- a + sum(Y)
  btilde <- b + 2 * sum(N) - sum(Y) # Note: b + sum(N - Y) would be more general
  lbeta(atilde, btilde) - lbeta(a, b) +
    sum(dgeom(N, xi, log = TRUE)) +
    sum(lchoose(N, Y))
}

# Importance sampling for N,phi conditionally on Y
#
# Usage:
#   arch_importance(K,Y,xi,a,b)
#
# Inputs:
#   Y,xi,a,b have the same meaning and syntax as for log_prob_NY(N,Y,xi,a,b)
#   The argument K defines the number of samples to generate.
#
# Output:
#  A data frame with K rows and J+1 columns, named N1, N2, etc,
#  for each j=1,…,J, containing samples from an importance sampling
#  distribution for Nj,
#  a column Phi with sampled phi-values,
#  and a final column called Log_Weights, containing re-normalised
#  log-importance-weights, constructed by shifting log(w[k]) by subtracting the
#  largest log(w[k]) value, as in lab 4.

arch_importance <- function(K, Y, xi, a, b) {
  Y <- as.matrix(Y)
  J <- nrow(Y)
  N_sample <- matrix(rep(pmax(Y[, 1], Y[, 2]), each = K), K, J)
  xi_sample <- 1 / (1 + 4 * N_sample)

  N <- matrix(rgeom(K * J, prob = xi_sample), K, J) + N_sample
  colnames(N) <- paste0("N", seq_len(J))

  Log_Weights <- vapply(
    seq_len(K),
    function(k) log_prob_NY(N[k, , drop = TRUE], Y, xi, a, b),
    0.0) -
    rowSums(dgeom(N - N_sample, prob = xi_sample, log = TRUE))

  phi <- rbeta(K, a + sum(Y), b + 2 * rowSums(N) - sum(Y))

  cbind(as.data.frame(N),
        Phi = phi,
        Log_Weights = Log_Weights - max(Log_Weights))
}
