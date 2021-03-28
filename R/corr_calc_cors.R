#' Calculates Spearman rank-based correlation and corresponding p values
#'
#' @param df1 Dataframe containing sample data, where each sample has its own row
#' @param df2 Second dataframe, against which df1 will be correlated
#' @param no.samples Number of samples being considered
#' @return A list containing the correlation matrix and its associated t-values
#'
#' @examples
#' x <- matrix(rnorm(200), ncol = 10, nrow = 20)
#' y <- matrix(rnorm(200), ncol = 10, nrow = 20)
#' results <- calc_cors(x, y, no.samples = 20)
#'
#' results$cors
#' results$ts
#'
#' @importFrom stats cor
#'
#' @export

calc_cors <- function(df1, df2, no.samples) {
  cors <- stats::cor(t(df1), t(df2), method = "spearman")
  ts <- (cors*sqrt(no.samples-2))/sqrt(1-cors^2)
  return(list(cors, ts))
}

