#' Greatest commond divisor (GCD)
#'
#' Calculate the greatest common divisor (GCD) of two numbers given the divisor
#' and the dividend. \cr \cr
#' You can find more information about the algorithm on: https://en.wikipedia.org/wiki/Euclidean_algorithm
#'
#' @param divisor a number.
#' @param dividend a number.
#' @return The greatest common divisor of \code{divisor} and \code{dividend}
#' @examples
#' calculate_gcd(10, 100)
#' calculate_gcd(327, 2389373)

calculate_gcd <- function(divisor, dividend) {

  if (!is.numeric(divisor) || !is.numeric(dividend)) {
    stop("Divisor and dividend have to be numerical.")
  }

  if (!is.atomic(divisor) || !is.atomic(dividend)) {
    stop("Divisor and dividend have to be atomic vectors.")
  }

  if (length(divisor) > 1 || length(dividend) > 1) {
    stop("Divisor and dividend can only be vectors with the length 1.")
  }

  if (divisor == 0) {
    stop("Not possible to divide by 0.")
  }

  if (divisor < 0 && dividend > 0) {
    stop("Divisor has to be greater or equal 0 for positive values of the dividend.")
  }

  residual <- dividend %% divisor
  if (residual == 0) {
    return(divisor)
  }
  else {
    calculate_gcd(residual, divisor)
  }
}
