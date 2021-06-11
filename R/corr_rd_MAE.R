#' Reading in a MultiAssayExperiment object
#' 
#' @param MAE A MultiAssayExperiment object
#' @param asys Character vector of assays to be selected for correlation analysis
#' @return A list containing the subsetted MultiAssayExperiment object, as well as the number of samples found in all assays specified
#' @examples 
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' results <- rd_MAE(toy_data, c("MicrobeGenetics", "hostExpression"))
#' 
#' results[[1]] # subsetted MultiAssayExperiment
#' results[[2]] # total number of samples
#' 
#' @import MultiAssayExperiment
#' 
#' @export

rd_MAE <- function(MAE, asys) {
  subMAE <- MAE[, , asys]
  subMAE <- intersectColumns(subMAE)
  no.samples <- colData(subMAE)@nrows
  return(list(subMAE, no.samples))
}