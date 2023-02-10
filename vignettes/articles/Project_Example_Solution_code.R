# Code for use in project 1
# This file should not be edited.
# Author: Finn Lindgren
# Load this file into your report.Rmd document using the method shown there.



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
