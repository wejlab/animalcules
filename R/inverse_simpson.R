#' Get alpha diversity using inverse simpson
#'
#' @param x A list of counts
#' @return A single value
#'
#' @examples
#' inverse_simpson(c(1:10))
#'
#' @export


# x: Species count vector
inverse_simpson <- function(x) {

    # Simpson index
    lambda <- simpson_index(x)

    # Inverse Simpson diversity
    (1/lambda)

}
