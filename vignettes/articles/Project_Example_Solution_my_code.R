# Finn Lindgren (s0000000, finnlindgren)



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
