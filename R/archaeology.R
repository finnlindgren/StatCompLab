#' Multiple binomial model likelihood
#'
#' Compute the log-likelihood for multiple Binom(N,phi) observations
#'
#' @param param Either a vector with two elements, N and phi, or
#'           a data.frame with two columns, named N and phi
#' @param y a data vector
#' @return
#'   The log-likelihood for each N,phi combination. The implementation
#'   internally uses the log-gamma function to provide a differentiable
#'   function of N, which allows optim() to treat N as a continuous variable.
#'   If N < max(y), the log-likelihood is defined to be -Inf.
#' @export
#' @examples
#' arch_loglike(
#'   param = data.frame(N = c(5, 10, 20, 40),
#'                      phi = c(0.1, 0.2, 0.3, 0.4)),
#'   y = c(2, 4))

arch_loglike <- function(param, y) {
  if (is.data.frame(param)) {
    ok <- param[["N"]] >= max(y)
    ll <- numeric(nrow(param))
    ll[!ok] <- -Inf
    for (yy in y) {
      ll[ok] <- ll[ok] +
        lchoose(param[["N"]][ok], yy) +
        yy * log(param[["phi"]][ok]) +
        (param[["N"]][ok] - yy) * log(1 - param[["phi"]][ok])
    }
    ll
  } else if (is.matrix(param)) {
    arch_loglike(data.frame(N = param[, 1],
                            phi = param[, 2]),
                 y)
  } else {
    sum(lchoose(param[1], y) +
          y * log(param[2]) +
          (param[1] - y) * log(1 - param[2]))
  }
}
