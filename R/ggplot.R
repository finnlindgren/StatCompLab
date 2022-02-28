# Idea from source: https://github.com/NicolasWoloszko/stat_ecdf_weighted/
# Code adapted from https://rdrr.io/github/tidyverse/ggplot2/src/R/stat-ecdf.r

#' Compute empirical weighted cumulative distribution
#'
#' Version of `ggplot2::stat_ecdf` that adds a `weights` property for each
#' observation, to produce an empirical weighted cumulative distribution function.
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualisation of distribution. Compared to other visualisations that rely on
#' density (like [geom_histogram()]), the ECDF doesn't require any
#' tuning parameters and handles both continuous and discrete variables.
#' The downside is that it requires more training to accurately interpret,
#' and the underlying visual tasks are somewhat more challenging.
#'
# @inheritParams layer
# @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' }
#' @seealso wquantile
#' @export
#' @examples
#' library(ggplot2)
#'
#' n <- 100
#' df <- data.frame(
#'   x = c(rnorm(n, 0, 10), rnorm(n, 0, 10)),
#'   g = gl(2, n),
#'   w = c(rep(1/n, n), sort(runif(n))^sqrt(n))
#' )
#' ggplot(df, aes(x, weights = w)) + stat_ewcdf(geom = "step")
#'
#' # Don't go to positive/negative infinity
#' ggplot(df, aes(x, weights = w)) + stat_ewcdf(geom = "step", pad = FALSE)
#'
#' # Multiple ECDFs
#' ggplot(df, aes(x, colour = g, weights = w)) + stat_ewcdf()
#' ggplot(df, aes(x, colour = g, weights = w)) +
#'   stat_ewcdf() +
#'   facet_wrap(vars(g), ncol = 1)

stat_ewcdf <- function(mapping = NULL, data = NULL,
                       geom = "step", position = "identity",
                       ...,
                       n = NULL,
                       pad = TRUE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEwcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}


#' @title StatEwcdf ggproto object
#' @name StatEwcdf
#' @rdname StatEwcdf
#' @aliases StatEwcdf
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 aes after_stat has_flipped_aes Stat
NULL

StatEwcdf <- ggplot2::ggproto(
  "StatEwcdf", ggplot2::Stat,
  required_aes = c("x|y", "weights"),

  default_aes = ggplot2::aes(y = ggplot2::after_stat(y)),

  setup_params = function(data, params) {
    params$flipped_aes <-
      ggplot2::has_flipped_aes(data,
                               params,
                               main_is_orthogonal = FALSE,
                               main_is_continuous = TRUE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      rlang::abort("stat_ewcdf() requires an x or y aesthetic.")
    }
    has_weights <- !(is.null(data$weights) && is.null(params$weights))
#    if (!has_weights) {
#      rlang::abort("stat_ewcdf() requires a weights aesthetic.")
#    }

    params
  },

  compute_group = function(data, scales, n = NULL, pad = TRUE, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }

    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    if (is.null(data$weights)) {
      data_ecdf <- ecdf(data$x)(x)
    } else {
      data_ecdf <-
        spatstat.geom::ewcdf(
          data$x,
          weights = data$weights / sum(data$weights)
        )(x)
    }

    df_ecdf <- vctrs::new_data_frame(list(x = x, y = data_ecdf), n = length(x))
    df_ecdf$flipped_aes <- flipped_aes
    ggplot2::flip_data(df_ecdf, flipped_aes)
  }
)





#' Weighted sample quantiles
#'
#' Calculates empirical sample quantiles with optional weights, for given
#' probabilities. Like in `quantile()`, the smallest observation corresponds to
#' a probability of 0 and the largest to a probability of 1.
#' Interpolation between discrete values is done when `type=7`, as in `quantile()`.
#' Use `type=1` to only generate quantile values from the raw input samples.
#'
#' @param x
#'   numeric vector whose sample quantiles are wanted.
#'  `NA` and `NaN` values are not allowed in numeric vectors unless `na.rm` is TRUE.
#' @param probs
#'   numeric vector of probabilities with values in \eqn{[0,1]}{[0,1]}.
#' @param na.rm
#'   logical; if true, any `NA` and `NaN`'s are removed from `x` before the
#'   quantiles are computed.
#' @param type numeric, 1 for no interpolation, or 7, for interpolated quantiles.
#'   Default is 7.
#' @param weights numeric vector of non-negative weights, the same length as
#' `x`, or `NULL`. The weights are normalised to sum to 1.
#'   If `NULL`, then `wquantile(x)` behaves the same as `quantile(x)`, with
#'   equal weight for each sample value.
#' @param ... Additional arguments, currently ignored
#' @seealso stat_ewcdf
#' @export
#' @examples
#' # Some random numbers
#' x <- rnorm(100)
#'
#' # Plain quantiles:
#' quantile(x)
#'
#' # Larger values given larger weight, on average shifting the quantiles upward:
#' wquantile(x, weights = sort(runif(length(x))))

wquantile <- function(x,
                      probs = seq(0, 1, 0.25),
                      na.rm = FALSE,
                      type = 7,
                      weights = NULL, ...) {
  if (is.null(weights) || (length(weights) == 1)) {
    weights <- rep(1, length(x))
  }
  stopifnot(all(weights >= 0))
  stopifnot(length(weights) == length(x))

  if (length(x) == 1) {
    return(rep(x, length(probs)))
  }

  n <- length(x)
  q <- numeric(length(probs))
  reorder <- order(x)
  weights <- weights[reorder]
  x <- x[reorder]
  wecdf <- pmin(1, cumsum(weights) / sum(weights))
  if (type == 1) {
  } else {
    weights2 <- (weights[-n] + weights[-1]) / 2
    wecdf2 <- pmin(1, cumsum(weights2) / sum(weights2))
  }
  for (pr_idx in seq_along(probs)) {
    pr <- probs[pr_idx]
    if (pr <= 0) {
      q[pr_idx] <- x[1]
    } else if (pr >= 1) {
      q[pr_idx] <- x[n]
    } else {
      if (type == 1) {
        j <- 1 + pmax(0, pmin(n - 1, sum(wecdf <= pr))) # 1 to n
        q[pr_idx] <- x[j]
      } else {
        j <- 1 + pmax(0, pmin(n - 2, sum(wecdf2 <= pr))) # 1 to n-1
        # Interpolate within each interval
        g <- (pr - c(0, wecdf2)[j]) / (wecdf2[j] - c(0, wecdf2)[j])
        # j + 1 is at most n
        q[pr_idx] <- (1 - g) * x[j] + g * x[j + 1]
      }
    }
  }

  q
}
