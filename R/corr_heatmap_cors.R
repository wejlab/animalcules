#' Create a plotly heatmap of correlations
#' 
#' @param cormat A matrix of significant correlations
#' @param hide_ax Option to hide axes. NA, 'xax' (x-axis), 'yax' (y-axis), or 'bax' (both axes)
#' 
#' @return A plotly heatmap of the signficant correlations
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
#' p <- heatmap_cors(sig_correlations)
#' p
#' 
#' @import heatmaply
#' 
#' @export

heatmap_cors <- function(cormat, hide_ax) {
  if (is.na(hide_ax)) {
    p <- heatmaply(cormat, scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "darkblue", 
                                                                                   high = "darkred", midpoint = 0, limits = c(-1, 1)))
  } else if (hide_ax == "xax") {
    p <- heatmaply(cormat, showticklabels = c(FALSE, TRUE), scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "darkblue", 
                                                                                                                    high = "darkred", midpoint = 0, limits = c(-1, 1)))
  } else if (hide_ax == "yax") {
    p <- heatmaply(cormat, showticklabels = c(TRUE, FALSE), scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "darkblue", 
                                                                                                                    high = "darkred", midpoint = 0, limits = c(-1, 1)))
  } else if (hide_ax == "bax") {
    p <- heatmaply(cormat, showticklabels = c(FALSE, FALSE), scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "darkblue", 
                                                                                                                     high = "darkred", midpoint = 0, limits = c(-1, 1)))
  }
  return(p)
}