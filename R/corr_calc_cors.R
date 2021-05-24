#' Calculates Spearman rank-based correlation and corresponding p values
#'
#' @param df1 Dataframe containing sample data, separating samples by columns
#' @param df2 Second dataframe, same format as df1
#' @param no.samples Number of samples being considered
#' @return A list containing the correlation matrix and its associated p-values
#'
#' @examples
#' x <- matrix(rnorm(200), ncol = 10, nrow = 20)
#' y <- matrix(rnorm(200), ncol = 10, nrow = 20)
#' results <- calc_cors(x, y, no.samples = 10)
#' results[[1]] # correlation matrix
#' results[[2]] # p-value matrix
#'
#' @importFrom stats cor
#'
#' @export

calc_cors <- function(df1, df2, no.samples) {
  # Correlation matrix, spearman correlations
  cors <- stats::cor(t(df1), t(df2), method = "spearman")
  # Calculate t-value
  ps <- abs((cors * sqrt(no.samples - 2))/sqrt(1 - cors^2))
  # convert t-value --> p-value, right sided test
  ps <- stats::pt(ps, df = no.samples - 2, lower.tail = FALSE)  # calculate p-value
  return(list(cors, ps))
}
