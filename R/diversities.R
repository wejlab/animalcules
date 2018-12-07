#' Get alpha diversity
#'
#' @param counts_table A dataframe with organism x sample
#' @param index one of inverse_simpson,gini_simpson,shannon,fisher,coverage
#' @param zeroes if ignore zero values
#' @return A list of alpha diversity
#'
#' @examples
#' diversities(matrix(1:12, nrow = 3),index="shannon")
#'
#' @export
diversities <- function(counts_table, index="all", zeroes=TRUE) {

    # Only include accepted indices
    index <- tolower(index)
    accepted <- c("inverse_simpson", "gini_simpson", "shannon",
                    "fisher", "coverage")

    # Return all indices
    if (length(index) == 1 && index == "all") {
        index <- accepted
    }

    if (!is.null(index)) {
        index <- intersect(index, accepted)
    }

    if (!is.null(index) && length(index) == 0) {
        return(NULL)
    }

    tab <- diversities_help(counts_table, index, zeroes)
    tab
}
