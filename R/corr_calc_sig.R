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
#' cors <- total_correlations[[1]]
#' ps <- total_correlations[[2]]
#' adjust <- "bonferroni"
#' no.sig <- 10
#' alpha <- 0.05
#' sig_correlations <- calc_sig(cors,
#'                              ps,
#'                              adjust,
#'                              no.sig,
#'                              alpha
#'                              )
#'
#' @export

calc_sig <- function(cors, ps, adjust, no.sig=1, alpha=0.05){
  # Pre-allocating memory
  # Empty correlation matrix
  sub_cors <- matrix(0,
                     nrow=nrow(cors),
                     ncol=ncol(cors))
  rownames(sub_cors) <- rownames(cors)
  colnames(sub_cors) <- colnames(cors)
  # Empty p-value matrix
  corrected_ps <- matrix(0,
                         nrow=nrow(ps),
                         ncol=ncol(ps))
  rownames(corrected_ps) <- rownames(ps)
  colnames(corrected_ps) <- colnames(ps)
  # applying selected p-value adjustment method
  for(i in 1:dim(ps)[2]){
    corrected_ps[,i] <- stats::p.adjust(ps[,i], method=adjust)
  }
  # Finding corrected p-values that satisfy significance threshold
  sig_ps <- data.frame(which(corrected_ps<=alpha, arr.ind = T))
  # Only selecting rows that have # sig. ps > no.sig
  sig_rows <- sig_ps %>%
    dplyr::count(row) %>%
    dplyr::filter(n>=no.sig)
  # 1) Extracting row + column indices from sig_rows + sig_ps
  # 2) finding them in cors
  # 3) putting them into sub_cors
  for(i in 1:length(sig_rows[,1])){
    r <- sig_rows[i,1]
    cols <- dplyr::filter(sig_ps, row==r)[,2]
    sub_cors[i, cols] <- cors[r, cols]
  }
  # trimming out all the 0's
  colz <- which(colSums2(sub_cors, na.rm = T)!=0)
  rowz <- which(rowSums2(sub_cors, na.rm=T)!=0)
  sub_cors <- sub_cors[rowz, colz]
  return(sub_cors)
}
