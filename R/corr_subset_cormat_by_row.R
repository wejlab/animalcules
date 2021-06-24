#' Subsetting a correlation matrix by user-selected rows
#' 
#' @param rows_selected Row indices selected by the user
#' @param summary Summary table produced by corr_func
#' @param cormat Correlation matrix produced by corr_func
#' @return A subsetted correlation matrix containing only the rows specified by the user
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
#'                      no.sig=1 # only want microbes correlated with =>20 genes
#'                      )
#' rows_selected <- c(1, 2, 3)
#' 
#' sub_cormat <- subset_cormat_by_row(rows_selected, results$summary, results$cormat)


subset_cormat_by_row <- function(rows_selected, summary, cormat){
  n <- length(rows_selected)
  otu <- summary[rows_selected, 1]
  grp <- summary[rows_selected, 3]
  sub_cormat <- NULL
  for(i in 1:n){
    o <- otu[i]
    g <- strsplit(grp[i], split = ";")[[1]]
    if(length(g)>1){
      sc <- cormat[rownames(cormat) %in% o,
                   colnames(cormat) %in% g] 
    } else {
      sc <- base::as.vector(cormat[rownames(cormat) %in% o,
                                   colnames(cormat) %in% g])
      names(sc) <- g
    }
    sub_cormat <- dplyr::bind_rows(sub_cormat, sc)
    sub_cormat[is.na(sub_cormat)] <- 0
  }
  rownames(sub_cormat) <- otu
  return(sub_cormat)
}