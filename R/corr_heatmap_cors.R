#' Plot heatmap given a correlation matrix
#' 
#' @param rows_selected Rows to plot as selected by the user 
#' @param summary Summary table from `corr_func`
#' @param cormat Correlation matrix from `corr_func`
#' @return Clustered heatmap, visualizing the correlations
#' 
#' @examples 
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' results <- corr_func(MAE = toy_data, 
#'                      asys = c('MicrobeGenetics', 'hostExpression'),
#'                      tax_level='genus',
#'                      correction='BH', # Benjamini-Hochberg
#'                      alpha=0.1,
#'                      no.sig=1
#'                      )
#'                      
#' rows_selected <- c(1, 2, 3)                      
#' p <- heatmap_cors(rows_selected, results$summary, results$cormat)
#' p
#' 
#' @import heatmaply
#' 
#' @export


heatmap_cors <- function(rows_selected, summary, cormat){
  sub_cormat <- subset_cormat_by_row(rows_selected, summary, cormat)
  if(nrow(sub_cormat) == 1){
    p <- heatmaply::heatmaply_cor(sub_cormat,
                                  Rowv = FALSE)
  } else {
    p <- heatmaply::heatmaply_cor(sub_cormat)
  }
  return(p)
}
