pretty_hist <- function(
    x,
    title = "Distribution of scores",
    colour = "pink") {

  if (!is.numeric(x) || !is.null(dim(x))) {
    stop("x must be a numeric vector")
  }

  x <- x[is.finite(x)]

  if (length(x) == 0) {
    stop("x must contain at least one finite value")
  }

  graphics::hist(
    x,
    col = colour,
    border = "white",
    main = title,
    xlab = "Score",
    ylab = "Frequency"
  )
}

z_score <- function(x) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    stop("x must be a numeric vector")
  }

  if (any(is.infinite(x))) {
    stop("x cannot contain infinite values")
  }

  score_sd <- stats::sd(x, na.rm = TRUE)

  if (is.na(score_sd) || score_sd == 0) {
    stop("At least two observed values with nonzero variation are needed")
  }

  return((x - mean(x, na.rm = TRUE)) / score_sd)
}

