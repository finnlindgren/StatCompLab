#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

optimisation.app <- function() {
  # Define UI for application that illustrates optimisation methods
  ui <- fluidPage(

    # Application title
    titlePanel("Optimisation"),

    # Sidebar with options and information
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(
          "target", "Function:",
          c(
            "Simple (1D)" = "simple.1d",
            "Simple (2D)" = "simple.2d",
            "Rosenbrock (2D)" = "rosenbrock",
            "Himmelblau (2D)" = "himmelblau",
            "Multimodal (1D)" = "multimodal.1d",
            "Multimodal (2D)" = "multimodal.2d",
            "Spiral (2D)" = "spiral"
          )
        ),
        selectInput(
          "method", "Method:",
          c(
            "Nelder-Mead Simplex" = "simplex",
            "Gradient Descent (with LS and adaptive step length)" = "gradient",
            "Newton (with LS)" = "newton",
            "Newton-BFGS (with LS)" = "bfgs"
          )
        ),
        actionButton("step.once", "Step"),
        actionButton("step.converge", "Converge"),
        actionButton("step.reset", "Reset"),
        hr(),
        verbatimTextOutput("info")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("targetPlot", click = "targetPlot_click", height = "auto", width = "100%", )
      )
    )
  )



  ## Keep track of the optimiser state, to allow redrawing the history and current step at will.
  ##

  get.fn <- function(target) {
    if (target == "simple.1d") {
      f.fn <- function(x) (exp(x - 1) - 1) ^ 2 + 2
      g.fn <- function(x, f) 2 * exp(x - 1) * (exp(x - 1) - 1)
      h.fn <- function(x, f) 2 * exp(x - 1) * (2 * exp(x - 1) - 1)
      xlim <- c(-2, 2)
      ylim <- NULL
      x.default <- rbind(-1)
      n.contour <- NULL
    } else if (target == "simple.2d") {
      f.fn <- function(x) (x[1, ] ^ 2 + x[2, ] ^ 2 + 1.5 * x[1, ] * x[2, ]) * 100 + 10
      g.fn <- function(x, f) {
        rbind(
          2 * x[1, ] + 1.5 * x[2, ],
          2 * x[2, ] + 1.5 * x[1, ]
        ) * 100
      }
      h.fn <- function(x, f) rbind(
          c(2, +1.5),
          c(+1.5, 2)
        ) * 100
      xlim <- c(-2.25, 2.25)
      ylim <- c(-1.5, 1.5)
      x.default <- rbind(0, 1.25)
      n.contour <- 40
    } else if (target == "rosenbrock") {
      f.fn <- function(x) 100 * (x[2, ] - x[1, ] ^ 2) ^ 2 + (1 - x[1, ]) ^ 2 - 1
      g.fn <- function(x, f) rbind(
        -400 * x[1, ] * (x[2, ] - x[1, ] ^ 2) - 2 * (1 - x[1, ]),
        200 * (x[2, ] - x[1, ] ^ 2)
      )
      h.fn <- function(x, f) rbind(
        c(1200 * x[1, ] ^ 2 - 400 * x[2, ] + 2, -400 * x[1, ]),
        c(-400 * x[1, ], 200)
      )
      xlim <- c(-1.5, 1.5)
      ylim <- c(-0.5, 1.5)
      x.default <- rbind(-0.5, 1)
      n.contour <- 50
    } else if (target == "himmelblau") {
      f.fn <- function(x) (x[1, ] ^ 2 + x[2, ] - 11) ^ 2 + (x[1, ] + x[2, ] ^ 2 - 7) ^ 2
      g.fn <- function(x, f) rbind(
        2 * (x[1, ] ^ 2 + x[2, ] - 11) * 2 * x[1, ] + 2 * (x[1, ] + x[2, ] ^ 2 - 7),
        2 * (x[1, ] ^ 2 + x[2, ] - 11) + 2 * (x[1, ] + x[2, ] ^ 2 - 7) * 2 * x[2, ]
      )
      h.fn <- function(x, f) rbind(
        c(
          2 * (2 * x[1, ]) * 2 * x[1, ] + 2 + 2 * (x[1, ] ^ 2 + x[2, ] - 11) * 2,
          2 * 2 * x[1, ] + 2 * 2 * x[2, ]
          ),
        c(
          2 * 2 * x[1, ] + 2 * 2 * x[2, ],
          2 + 2 * 2 * x[2, ] * 2 * x[2, ] + 2 * (x[1, ] + x[2, ] ^ 2 - 7) * 2
        ))
      xlim <- c(-5, 5)
      ylim <- c(-5, 5)
      x.default <- rbind(0.134, -5)
      n.contour <- 50
    } else if (target == "multimodal.1d") {
      f.fn <- function(x) sin(x * 2 * pi) + x ^ 2
      g.fn <- NULL
      h.fn <- NULL
      xlim <- c(-2, 2)
      ylim <- NULL
      x.default <- rbind(-0.5)
      n.contour <- NULL
    } else if (target == "multimodal.2d") {
      f.fn <- function(x) (sin(x[1, ] * 2 * pi) * cos(x[2, ] * 2 * pi) + x[1, ] ^ 2 * 3 + x[2, ] ^ 2 * 3 + x[1, ] * x[2, ]) * 10
      g.fn <- NULL
      h.fn <- NULL
      xlim <- c(-3 / 2, 3 / 2)
      ylim <- c(-1, 1)
      x.default <- rbind(-0.7, 1)
      n.contour <- 50
    } else if (target == "spiral") {
      f.fn <- function(x) {
        ## From Simon Wood
        r <- colSums(x ^ 2) ^ 0.5
        theta <- atan2(x[2, ], x[1, ])
        m <- ceiling(r - theta / (2 * pi))
        ru <- m + theta / (2 * pi) ## upper bounding r
        theta <- theta + m * (2 * pi) ## sidtance along spiral
        z <- ru - r ## distance from upper bound to point
        zz <- theta - 4 * pi + 1
        lb <- -1 - exp(-zz ^ 2 / (4 * pi))
        z ^ 2 * (1 - z) ^ 2 * lb * 100
      }
      g.fn <- NULL
      h.fn <- NULL
      xlim <- c(-2, 2)
      ylim <- c(-4 / 3, 4 / 3)
      x.default <- rbind(-0.4, 0)
      n.contour <- 40
    }
    if (nrow(x.default) == 1) {
      if (is.null(g.fn)) {
        g.fn <- function(x, f) {
          (f(x + 1e-6) - f(x - 1e-6)) / 2e-6
        }
      }
      if (is.null(h.fn)) {
        h.fn <- function(x, f) {
          (f(x + 1e-4) - 2 * f(x) + f(x - 1e-4)) / 1e-8
        }
      }
    } else {
      if (is.null(g.fn)) {
        g.fn <- function(x, f) {
          e1 <- rbind(1e-6, 0)
          e2 <- rbind(0, 1e-6)
          rbind(f(x + e1) - f(x - e1), f(x + e2) - f(x - e2)) / 2e-6
        }
      }
      if (is.null(h.fn)) {
        h.fn <- function(x, f) {
          e1 <- rbind(1e-4, 0)
          e2 <- rbind(0, 1e-4)
          f12 <- f(x + e1 / 2 + e2 / 2) + f(x - e1 / 2 - e2 / 2) - f(x + e1 / 2 - e2 / 2) - f(x - e1 / 2 + e2 / 2)
          rbind(
            cbind(f(x + e1) - 2 * f(x) + f(x - e1), f12),
            cbind(f12, f(x + e2) - 2 * f(x) + f(x - e2))
          ) / 1e-8
        }
      }
    }
    list(
      f = f.fn, g = g.fn, h = h.fn, xlim = xlim, ylim = ylim, x.default = x.default,
      n.contour = n.contour
    )
  }


  take.a.step <- function(state, fn, method) {
    if (method == "simplex") {
      dims <- nrow(state$x)
      if (is.null(state$meta)) {
        ## Initialise the simplex method
        if (dims == 1) {
          state$meta <- list(
            simplex = cbind(state$x, state$x + 0.1),
            f = numeric(2),
            size = Inf
          )
          state$meta$f <- c(
            fn$f(state$meta$simplex[, 1, drop = FALSE]),
            fn$f(state$meta$simplex[, 2, drop = FALSE])
          )
          state$n$f <- state$n$f + 1
        } else {
          state$meta <- list(
            simplex = cbind(state$x, state$x + c(0.1, 0), state$x + c(0, 0.1)),
            f = numeric(3),
            size = Inf
          )
          state$meta$f <- c(
            fn$f(state$meta$simplex[, 1, drop = FALSE]),
            fn$f(state$meta$simplex[, 2, drop = FALSE]),
            fn$f(state$meta$simplex[, 3, drop = FALSE])
          )
          state$n$f <- state$n$f + 2
        }
        o <- order(state$meta$f)
        state$meta$f <- state$meta$f[o]
        state$meta$simplex <- state$meta$simplex[, o, drop = FALSE]
        state$meta$simplex.history <- list(state$meta$simplex)
      }
      ## Update simplex
      best.centroid <- rowMeans(state$meta$simplex[, -(dims + 1), drop = FALSE])
      x.new <- best.centroid + (best.centroid - state$meta$simplex[, dims + 1, drop = FALSE])
      f.new <- fn$f(x.new)
      state$n$f <- state$n$f + 1
      if (f.new < min(state$meta$f)) { ## New best point. Try expansion.
        x.ext <- best.centroid + 2 * (x.new - best.centroid)
        f.ext <- fn$f(x.ext)
        state$n$f <- state$n$f + 1
        if (f.ext < f.new) {
          state$meta$simplex[, dims + 1] <- x.ext
          state$meta$f[dims + 1] <- f.ext
        } else {
          state$meta$simplex[, dims + 1] <- x.new
          state$meta$f[dims + 1] <- f.new
        }
      } else if (f.new > state$meta$f[dims]) { ## Not sufficient improvement. Contract.
        f.threshold <- min(f.new, state$meta$f[dims + 1])
        gamma <- 0.5
        if (f.new > state$meta$f[dims + 1]) {
          x.contract <- best.centroid + gamma * (state$meta$simplex[, dims + 1, drop = FALSE] - best.centroid)
        } else {
          x.contract <- best.centroid + gamma * (x.new - best.centroid)
        }
        f.contract <- fn$f(x.contract)
        state$n$f <- state$n$f + 1
        if (f.contract < f.threshold) {
          state$meta$simplex[, dims + 1] <- x.contract
          state$meta$f[dims + 1] <- f.contract
        } else { ## Shrink towards the best point
          gamma <- gamma * 1.0
          state$meta$simplex[, 2] <- (gamma * state$meta$simplex[, 1, drop = FALSE] +
            (1 - gamma) * state$meta$simplex[, 2, drop = FALSE])
          state$meta$f[2] <- fn$f(state$meta$simplex[, 2, drop = FALSE])
          state$n$f <- state$n$f + 1
          if (dims > 1) {
            state$meta$simplex[, 3] <- (gamma * state$meta$simplex[, 1, drop = FALSE] +
              (1 - gamma) * state$meta$simplex[, 3, drop = FALSE])
            state$meta$f[3] <- fn$f(state$meta$simplex[, 3, drop = FALSE])
            state$n$f <- state$n$f + 1
          }
        }
      } else {
        state$meta$simplex[, dims + 1] <- x.new
        state$meta$f[dims + 1] <- f.new
      }
      ## Order the simplex
      o <- order(state$meta$f)
      state$meta$f <- state$meta$f[o]
      state$meta$simplex <- state$meta$simplex[, o, drop = FALSE]
      state$x <- state$meta$simplex[, 1, drop = FALSE]
      ## Save simplex history
      state$meta$simplex.history <- c(state$meta$simplex.history, list(state$meta$simplex))
      ## Check convergence
      if (dims == 1) {
        state$meta$size <- abs(diff(range(state$meta$simplex[1, ])))
      } else {
        state$meta$size <- max(
          abs(diff(range(state$meta$simplex[1, ]))),
          abs(diff(range(state$meta$simplex[2, ])))
        )
      }
      state$converged <- state$meta$size < 1e-6
      return(state)
    } else if (method == "gradient") {
      if (is.null(state$meta)) {
        ## Initialise the gradient descent method
        state$meta <- list(step.length = 0.1, factor = 2 / 3)
      }
      ## A proper implementation would already have the gradient, so don't count it
      g0 <- fn$g(state$x, fn$f)
      if (state$n$g == 0) {
        state$n$g <- 1
      }
      step.length <- state$meta$step.length
      search.dir <- -g0 / sqrt(sum(g0 ^ 2))
    } else if (method == "newton") {
      H <- fn$h(state$x, fn$f)
      state$n$h <- state$n$h + 1
      EH <- eigen(H)
      H <- EH$vectors %*% diag(pmax(abs(EH$values), 1e-6), nrow = nrow(state$x), ncol = nrow(state$x)) %*% t(EH$vectors)
      ## A proper implementation would already have the gradient, so don't count it
      g0 <- fn$g(state$x, fn$f)
      if (state$n$g == 0) {
        state$n$g <- 1
      }
      search.dir <- -solve(H, g0)
      step.length <- 1
    } else if (method == "bfgs") {
      if (is.null(state$Hinv)) {
        ## Initialise the inverse Hessian
        H <- fn$h(state$x, fn$f)
        state$n$h <- state$n$h + 1
        EH <- eigen(diag(diag(H), nrow = nrow(H), ncol = ncol(H)))
        state$Hinv <- EH$vectors %*% diag(
          1 / pmax(abs(EH$values), 1e-6),
          nrow = nrow(state$x),
          ncol = nrow(state$x)
        ) %*% t(EH$vectors)
      }
      ## A proper implementation would already have the gradient, so don't count it
      g0 <- fn$g(state$x, fn$f)
      if (state$n$g == 0) {
        state$n$g <- 1
      }
      search.dir <- -state$Hinv %*% g0
      step.length <- 1
    }
    ## Do linesearch
    search.max <- 10
    f0 <- fn$f(state$x) ## A proper implementation would already have this, so don't count it
    Dg0 <- sum(g0 * search.dir)
    f1 <- fn$f(state$x + step.length * search.dir)
    state$n$f <- state$n$f + 1
    search.loop <- 1
    improvement <- f0 + Dg0 * step.length * 1e-6 > f1
    if (improvement && (method == "gradient")) {
      state$meta$step.length <- state$meta$step.length / state$meta$factor
    }
    while (!improvement && (search.loop <= search.max)) {
      search.loop <- search.loop + 1
      ## f0 + Dg0*step.length + h*step.length^2/2 = f1
      ##   solved by
      ## h = (f1-f0-Dg0*step.length)/(step.length^2/2)
      ##   and minimised at
      ## step.length = -Dg0/h = -step.length^2/2*Dg0/(f1-f0-Dg0*step.length)
      step.length <- max(step.length / 4, -step.length ^ 2 / 2 * Dg0 / (f1 - f0 - Dg0 * step.length))
      f1 <- fn$f(state$x + step.length * search.dir)
      state$n$f <- state$n$f + 1
      improvement <- f0 + Dg0 * step.length * 1e-6 > f1
    }
    state$x <- state$x + step.length * search.dir
    if (method == "gradient") {
      if (step.length < state$meta$step.length * state$meta$factor) {
        state$meta$step.length <- state$meta$step.length * state$meta$factor
      }
    }
    g1 <- fn$g(state$x, fn$f)
    state$n$g <- state$n$g + 1
    state$converged <- sum(g1 ^ 2) ^ 0.5 < 1e-6
    if (method == "bfgs") {
      s1 <- step.length * search.dir
      y1 <- g1 - g0
      sy1 <- sum(s1 * y1)
      rho <- 1 / sy1
      tmp <- state$Hinv - rho * (s1 %*% (t(y1) %*% state$Hinv))
      state$Hinv <- tmp - rho * ((tmp %*% y1) %*% t(s1)) + abs(rho) * (s1 %*% t(s1))
    }
    state
  }

  ad.quad <- function(f0, g, H, x, xlim, ylim, col=2, trans=function(x) log10(abs(x + 1)), lev=1:10 / 2, lty=1) {
    ## function to add a quadratic approximation to a contour
    ## plot of a function, based on a Taylor expansion
    ## at x,z
    qap <- function(x, y, f0, g, H) {
      X <- matrix(c(x, y), length(x), 2)
      f0 + X %*% g + rowSums((X %*% H) * X) / 2
    }
    n <- 50
    xx <- seq(xlim[1], xlim[2], length = n)
    yy <- seq(ylim[1], ylim[2], length = n)
    q <- trans(outer(xx - x[1], yy - x[2], qap, f0 = f0, g = g, H = H))
    ##  contour(xx,yy,matrix(q,n,n),col=col,add=TRUE,levels=lev,lty=lty)
    contour(xx, yy, matrix(q, n, n), col = col, levels = lev, add = TRUE, lty = lty)
  }



  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    state <- reactiveValues(
      x.state = list(
        x = get.fn("simple.1d")$x.default,
        converged = FALSE,
        n = list(f = 1, g = 0, h = 0)
      ),
      x.start = get.fn("simple.1d")$x.default,
      x.history = matrix(get.fn("simple.1d")$x.default, 1, 1),
      fn = get.fn("simple.1d")
    )

    observeEvent(input$target, {
      fn <- get.fn(input$target)
      state$fn <- fn
      state$x.start <- fn$x.default
      state$x.history <- fn$x.default
      state$x.state <- list(x = fn$x.default, converged = FALSE, n = list(f = 1, g = 0, h = 0))
    })

    observeEvent(input$method, {
      state$x.history <- state$x.start
      state$x.state <- list(x = state$x.start, converged = FALSE, n = list(f = 1, g = 0, h = 0))
    })

    observeEvent(input$step.once, {
      if (!state$x.state$converged) {
        state$x.state <- take.a.step(state$x.state, fn = state$fn, method = input$method)
        state$x.history <- cbind(state$x.history, state$x.state$x)
      }
    })
    observeEvent(input$step.converge, {
      step.max <- (floor((ncol(state$x.history) - 1) / 500) + 1) * 500
      while (!state$x.state$converged && (ncol(state$x.history) - 1 < step.max)) {
        state$x.state <- take.a.step(state$x.state, fn = state$fn, method = input$method)
        state$x.history <- cbind(state$x.history, state$x.state$x)
      }
    })
    observeEvent(input$step.reset, {
      state$x.history <- state$x.start
      state$x.state <- list(x = state$x.start, converged = FALSE, n = list(f = 1, g = 0, h = 0))
    })

    observeEvent(input$targetPlot_click, {
      if (nrow(state$x.start) == 1) {
        x.start <- rbind(input$targetPlot_click$x)
      } else {
        x.start <- rbind(input$targetPlot_click$x, input$targetPlot_click$y)
      }
      state$x.start <- x.start
      state$x.history <- x.start
      state$x.state <- list(x = x.start, converged = FALSE, n = list(f = 1, g = 0, h = 0))
    })



    output$info <- renderText({
      xy_str <- function(e) {
        if (is.null(e)) {
          return("NULL\n")
        }
        if (length(e) == 1) {
          paste0("x=", signif(e[1], 6), "\n")
        } else {
          paste0("x=", signif(e[1], 6), ", y=", signif(e[2], 6), "\n")
        }
      }

      if (input$method == "simplex") {
        if (is.null(state$x.state$meta)) {
          convergence.str <- paste0(
            "|simplex| = ",
            signif(Inf, 6)
          )
        } else {
          convergence.str <- paste0(
            "|simplex| = ",
            signif(state$x.state$meta$size, 6)
          )
        }
      } else {
        convergence.str <- paste0(
          "|g(x)| = ",
          signif(sum(state$fn$g(state$x.state$x, state$fn$f) ^ 2) ^ 0.5, 6)
        )
      }
      paste0(
        "Start:       ", xy_str(state$x.start),
        "Current:     ", xy_str(state$x.state$x),
        "Values:      ",
        "f(x) = ", signif(state$fn$f(state$x.state$x), 6),
        "\n             ", convergence.str, "\n",
        "Iteration:   ", ncol(state$x.history) - 1, "\n",
        "Converged:   ", state$x.state$converged, "\n",
        "Evaluations: ",
        "#f = ", state$x.state$n$f,
        "\n             #gradient = ", state$x.state$n$g,
        "\n             #hessian = ", state$x.state$n$h
      )
    })


    output$targetPlot <- renderPlot(
      {
        main <- paste0(
          "Iter: ", ncol(state$x.history) - 1,
          ", Converged: ", c("FALSE", "TRUE")[1 + state$x.state$converged]
        )
        if (nrow(state$fn$x.default) == 1) {
          xlim <- range(state$fn$xlim, state$x.history[1, ])
          curve(
            state$fn$f(x), state$fn$xlim[1], state$fn$xlim[2],
            xlim = xlim,
            xlab = "x", ylab = "Target function", main = main
          )
        } else {
          xlim <- range(state$fn$xlim, state$x.history[1, ])
          ylim <- range(state$fn$ylim, state$x.history[2, ])
          xx <- seq(state$fn$xlim[1], state$fn$xlim[2], length = 100)
          yy <- seq(state$fn$ylim[1], state$fn$ylim[2], length = 100)
          xygrid <- expand.grid(xx, yy)
          contour(
            xx, yy, matrix(state$fn$f(t(xygrid)), length(xx), length(yy)),
            n = state$fn$n.contour,
            xlim = xlim, ylim = ylim,
            xlab = "x", ylab = "y", main = main,
            asp = 1
          )
        }
        if (ncol(state$x.history) > 0) {
          if (nrow(state$x.history) == 1) {
            curr_x <- state$x.state$x
            curr_x_v <- as.vector(curr_x)
            fval <- vapply(
              seq_len(ncol(state$x.history)),
              function(idx) state$fn$f(state$x.history[1, idx]),
              1.0
            )
            points(state$x.history[1, ], fval, pch = 19, col = 6)
            if (input$method == "simplex") {
              if (!is.null(state$x.state$meta)) {
                lines(state$x.state$meta$simplex[1, , drop = TRUE],
                      state$x.state$meta$f, col = 2)
              }
            } else if (input$method == "gradient") {
              curve(
                as.vector(state$fn$f(curr_x)) +
                  (x - curr_x_v) * as.vector(state$fn$g(curr_x, state$fn$f)),
                add = TRUE, col = 2
              )
            } else if (input$method == "newton") {
              H <- state$fn$h(curr_x, state$fn$f)
              EH <- eigen(H)
              H2 <- EH$vectors %*% diag(
                pmax(abs(EH$values), 1e-6),
                nrow = nrow(curr_x),
                ncol = nrow(curr_x)
              ) %*% t(EH$vectors)
              curve(as.vector(state$fn$f(curr_x)) +
                      (x - curr_x_v) * as.vector(state$fn$g(curr_x, state$fn$f)) +
                        (x - curr_x_v) ^ 2 / 2 * as.vector(H), add = TRUE, col = 2)
              curve(as.vector(state$fn$f(curr_x)) +
                      (x - curr_x_v) * as.vector(state$fn$g(curr_x, state$fn$f)) +
                        (x - curr_x_v) ^ 2 / 2 * as.vector(H2), add = TRUE, col = 4)
            } else if (input$method == "bfgs") {
              H <- state$fn$h(curr_x, state$fn$f)
              if (is.null(state$x.state$Hinv)) {
                EH <- eigen(H)
                H2 <- EH$vectors %*% diag(
                  pmax(abs(EH$values), 1e-6),
                  nrow = nrow(curr_x),
                  ncol = nrow(curr_x)
                ) %*% t(EH$vectors)
              } else {
                H2 <- solve(state$x.state$Hinv)
              }
              curve(as.vector(state$fn$f(curr_x)) +
                      (x - curr_x_v) * as.vector(state$fn$g(curr_x, state$fn$f)) +
                        (x - curr_x_v) ^ 2 / 2 * as.vector(H), add = TRUE, col = 2)
              curve(as.vector(state$fn$f(curr_x)) +
                      (x - curr_x_v) * as.vector(state$fn$g(curr_x, state$fn$f)) +
                        (x - curr_x_v) ^ 2 / 2 * as.vector(H2), add = TRUE, col = 4)
            }
          } else {
            curr_x <- state$x.state$x
            curr_x_v <- as.vector(curr_x)
            if (input$method == "simplex") {
              if (!is.null(state$x.state$meta)) {
                lapply(
                  state$x.state$meta$simplex.history,
                  function(x) lines(t(x[, c(1, 2, 3, 1), drop = FALSE]), col = 4)
                )
                lines(t(state$x.state$meta$simplex[, c(1, 2, 3, 1), drop = FALSE]), col = 2)
              }
            } else if (input$method == "gradient") {
              gg <- state$fn$g(curr_x, state$fn$f)
              gg <- -gg / sqrt(sum(gg ^ 2)) * ifelse(is.null(state$x.state$meta),
                0.1,
                state$x.state$meta$step.length
              )
              lines(t(cbind(curr_x, curr_x + gg)), col = 2)
            } else if (input$method == "newton") {
              f0 <- state$fn$f(curr_x)
              gg <- state$fn$g(curr_x, state$fn$f)
              H <- state$fn$h(curr_x, state$fn$f)
              EH <- eigen(H)
              H2 <- EH$vectors %*% diag(
                pmax(abs(EH$values), 1e-6),
                nrow = nrow(curr_x),
                ncol = nrow(curr_x)
              ) %*% t(EH$vectors)
              ad.quad(f0, g = gg, H = H, x = curr_x, state$fn$xlim, state$fn$ylim, col = 2)
              ad.quad(f0, g = gg, H = H2, x = curr_x, state$fn$xlim, state$fn$ylim, col = 4)
            } else if (input$method == "bfgs") {
              f0 <- state$fn$f(curr_x)
              gg <- state$fn$g(curr_x, state$fn$f)
              H <- state$fn$h(curr_x, state$fn$f)
              if (is.null(state$x.state$Hinv)) {
                EH <- eigen(diag(diag(H), ncol = ncol(H)))
                H2 <- EH$vectors %*% diag(
                  pmax(abs(EH$values), 1e-6),
                  nrow = nrow(curr_x),
                  ncol = nrow(curr_x)
                ) %*% t(EH$vectors)
              } else {
                H2 <- solve(state$x.state$Hinv)
              }
              ad.quad(f0, g = gg, H = H, x = curr_x, state$fn$xlim, state$fn$ylim, col = 2)
              ad.quad(f0, g = gg, H = H2, x = curr_x, state$fn$xlim, state$fn$ylim, col = 4)
            }
            points(state$x.history[1, ], state$x.history[2, ], pch = 19, col = 6)
            lines(state$x.history[1, ], state$x.history[2, ], pch = 19, col = 6)
          }
        }
      },
      height = function() {
        max(400, session$clientData$output_targetPlot_width * 0.75)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}


#' Interactive numerical optimisation explorer
#'
#' Shiny app for exploring how several numerical optimisation methods work
#' @export
#' @import shiny
optimisation <- function() {
  shiny::runApp(optimisation.app())
}
