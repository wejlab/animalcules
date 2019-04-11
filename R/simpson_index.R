#' Get alpha diversity using simpson
#'
#' @param x A list of counts
#' @return A single value
#'
#' @examples
#' simpson_index(c(1:10))
#'
#' @export

simpson_index <- function(x) {
    # Relative abundances
    p <- x/sum(x)

    # Simpson index
    lambda <- sum(p^2)

    lambda
}
