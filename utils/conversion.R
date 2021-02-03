convert_rmd <- function(input_filename,
                        type = NULL,
                        solutions = FALSE,
                        output_filename = NULL) {
  new_type <- match.arg(type, c("vignette", "tutorial"))

  orig_name <- basename(input_filename)
  orig_name <- gsub(pattern = "\\.Rmd",
                          replacement = "",
                          x = orig_name)

  # Parse input file
  rmd <- readr::read_file(input_filename)
  rmd_split <- strsplit(rmd, "---")
  config <- yaml::read_yaml(text = rmd_split[[1]][2])

  # Detect output type
  if (identical(config[["output"]],
                "learnr::tutorial")) {
    orig_type <- "tutorial"
  } else if (identical(config[["output"]],
                       "rmarkdown::html_vignette")) {
    orig_type <- "vignette"
  } else {
    stop("Unknown original type")
  }
  # Detect .vignette and .solutions flags
  orig_tutorial <- grepl(pattern = "\\n\\.tutorial <- TRUE\\n", rmd)
  orig_solutions <- grepl(pattern = "\\n\\.solutions <- TRUE\\n", rmd)

  if (identical(new_type, orig_type) &&
      identical(solutions, orig_solutions)) {
    warning("Input of same type as output. Nothing to do.")
  }

  # Construct output filename
  if (is.null(output_filename)) {
    new_name <- paste0(
      orig_name,
      if (solutions) {
        "Solutions"
      } else {
        NULL
      }
    )
    if (new_type == "tutorial") {
      output_filename <-
        file.path(
          "inst/tutorials",
          new_name,
          paste0(new_name, ".Rmd")
        )
    }
    if (new_type == "vignette") {
      output_filename <-
        file.path(
          "vignettes",
          paste0(new_name, ".Rmd")
        )
    }

  }
  new_dir <- dirname(output_filename)
  new_name <- basename(output_filename)
  new_name <- gsub(pattern = "\\.Rmd",
                   replacement = "",
                   x = new_name)

  config$title <-
    paste0(
      config[["title"]],
      if (solutions && !orig_solutions) {
        " (solutions)"
      } else {
        NULL
      }
    )

  if (new_type == "tutorial") {
    config$output <- "learnr::tutorial"
    config$runtime <- "shiny_prerendered"
    if (is.null(config$tutorial)) {
      config$tutorial <- list()
    }
    config$tutorial$id =
      paste0(
        "shinyapps.finnlindgren.StatComp",
        orig_name
      )
  } else {
    config$runtime <- NULL
  }

  if (new_type == "vignette") {
      config$output <- "rmarkdown::html_vignette"
      config$vignette <-
        paste0("%\\VignetteIndexEntry{",
               config[["title"]],
               "}\n",
               "%\\VignetteEngine{knitr::rmarkdown}\n",
               "%\\VignetteEncoding{UTF-8}\n")
  } else {
    config$vignette <- NULL
  }

  rmd_split[[1]][2] <- yaml::as.yaml(config)
  new_rmd <- paste0(rmd_split[[1]], collapse = "---\n")

  new_rmd <- sub(pattern = "\\n\\.tutorial <- [^\\n]*\\n",
                 replacement =
                   paste0("\n.tutorial <- ",
                          identical(new_type, "tutorial"),
                          "\n"),
                 new_rmd)
  new_rmd <- sub(pattern = "\\n\\.solutions <- [^\\n]*\\n",
                 replacement =
                   paste0("\n.solutions <- ",
                          solutions,
                          "\n"),
                 new_rmd)

  if (!dir.exists(dirname(output_filename))) {
    dir.create(dirname(output_filename))
  }
  write(new_rmd, file = output_filename)

  # Copy resources
  if (identical(orig_type, "tutorial")) {
    resource_dirs <- c("images")
    for (res_dir in resource_dirs) {
      in_dir <- file.path(
        dirname(input_filename),
        res_dir)
      out_dir <- file.path(
        dirname(output_filename),
        res_dir)
      if (dir.exists(in_dir)) {
        if (!dir.exists(out_dir)) {
          dir.create(out_dir)
        }
        for (file in list.files(in_dir)) {
          file.copy(file.path(in_dir, file),
                    file.path(out_dir, file))
        }
      }
    }
  }

  invisible(NULL)
}

convert_tutorial_rmd <- function(input_filename) {
  convert_rmd(input_filename, type = "tutorial", solutions = TRUE)
  convert_rmd(input_filename, type = "vignette", solutions = FALSE)
  convert_rmd(input_filename, type = "vignette", solutions = TRUE)
}
