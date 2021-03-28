#' Calculate significant correlations, given hostExpression data and microbial abundance data
#'
#' @param cors A matrix of correlation coefficients
#' @param ts A matrix of t-values, associated with each correlation coefficient in cors
#' @param no.sig Minimum number of significant correlations to be extracted
#' @param t_crit Critical t-value for determining significance
#' @return Subset of cors, containing only the correlation coefficients that are significant
#'
#' @examples
#' x <- matrix(rnorm(200), ncol = 10, nrow = 20)
#' y <- matrix(rnorm(200), ncol = 10, nrow = 20)
#' no.samples <- 20
#' total_correlations <- calc_cors(x, y, no.samples = 20)
#' cors <- total_correlations$cors
#' ts <- total_correlations$ts
#' alpha <- 0.05
#' t_crit <- abs(stats::qt(alpha, no.samples-2))
#' sig_correlations <- calc_sig(cors, ts, no.sig, t_crit)
#'
#' @export

calc_sig <- function(cors, ts, no.sig, t_crit){
  # Creating empty, subset correlation matrix, same dimensions as cors
  sub_cors <- matrix(0,
                     nrow=nrow(cors),
                     ncol=ncol(cors))
  rownames(sub_cors) <- rownames(cors)
  colnames(sub_cors) <- colnames(cors)
  # Finding indices of t-values above t_crit (abs(ts) is done beforehand)
  sig_ts <- which(ts>=t_crit, arr.ind = T)
  sig_ts <- as.data.frame(sig_ts)
  # Only selecting rows that have no. significant t-values > no.sig
  sig_rows <- sig_ts %>%
    dplyr::count(row) %>%
    dplyr::filter(n>=no.sig)
  # Extracting row + column indices from sig_rows + sig_ts,
  # finding them in cors,
  # putting them in sub_cors
  for(i in 1:length(sig_rows[,1])){
    r <- sig_rows[i,1]
    cols <- dplyr::filter(sig_ts, row==r)[,2]
    sub_cors[i, cols] <- cors[r, cols]
  }
  # trimming out all the 0's
  colz <- which(colSums2(sub_cors, na.rm = T)!=0)
  rowz <- which(rowSums2(sub_cors, na.rm=T)!=0)
  tc <- sub_cors[rowz, colz]
  return(tc)
}
