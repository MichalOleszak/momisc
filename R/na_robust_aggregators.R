#' Minimal value ignoring NAa
#'
#' @param x numeric vector
#'
#' @return A single numeric.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, NA, 5)
#' min_(a)
min_ <- function(x) {
  out <- min(x, na.rm = TRUE)
  return(out)
}

#' Maximal value ignoring NAa
#'
#' @param x numeric vector
#'
#' @return A single numeric.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, NA, 5)
#' max_(a)
max_ <- function(x) {
  out <- max(x, na.rm = TRUE)
  return(out)
}

#' Sum of values ignoring NAa
#'
#' @param x numeric vector
#'
#' @return A single numeric.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, NA, 5)
#' sum_(a)
sum_ <- function(x) {
  out <- sum(x, na.rm = TRUE)
  return(out)
}

#' Arithmetic mean of values ignoring NAa
#'
#' @param x numeric vector
#'
#' @return A single numeric.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, NA, 5)
#' mean_(a)
mean_ <- function(x) {
  out <- mean(x, na.rm = TRUE)
  return(out)
}

#' Median of values ignoring NAs
#'
#' @param x numeric vector
#'
#' @return A single numeric.
#' @export
#'
#' @examples
#' a <- c(1, 2, 3, NA, 5)
#' median_(a)
median_ <- function(x) {
  out <- median(x, na.rm = TRUE)
  return(out)
}
