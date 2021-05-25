#' Plot heatmap given a correlation matrix
#' 
#' @param cormat Correlation matrix with significant correlations
#' @param hide_ax Which axis to hide
#' 
#' @return Clustered heatmap, visualizing the correlations
#' 
#' @examples 
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' results <- corr_func(MAE = toy_data, 
#'                      asys = c('MicrobeGenetics', 'HostGenetics'),
#'                      tax_level='genus',
#'                      correction='BH', # Benjamini-Hochberg
#'                      alpha=0.1,
#'                      no.sig=1 # only want microbes correlated with =>20 genes
#'                      )
#' p <- heatmap_cors(results$cormat, hide_ax=NA)
#' p
#' 
#' @import heatmaply
#' 
#' @export


heatmap_cors <- function(cormat, hide_ax){
  if(is.na(hide_ax)){
    p <- heatmaply_cor(cormat)
  }else if(hide_ax == "xax") {
    p <- heatmaply_cor(cormat,
                       showticklabels = c(FALSE, TRUE))
  } else if(hide_ax == "yax"){
    p <- heatmaply_cor(cormat,
                       showticklabels = c(TRUE, FALSE))
  } else if(hide_ax == "bax"){
    p <- heatmaply_cor(cormat,
                       plot_method="plotly",
                       showticklabels = c(FALSE, FALSE))
  }
  return(p)
}