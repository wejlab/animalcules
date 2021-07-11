#' Inserting GSVA scores into a MultiAssayExperiment
#' 
#' @param MAE MultiAssayExperiment object
#' @param gsva_data ssGSEA scores calculated from the path_proj function
#' @returns Updated MAE containing the ssGSEA scores for the user-selected geneset
#' 
#' @examples 
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' gsva_results <- path_proj(toy_data, "H", NA) # H = Hallmark gene sets
#' 
#' test_MAE <- insert_into_MAE(toy_data, gsva_results)
#' 
#' @export

insert_into_MAE <- function(MAE, gsva_data){
  nms <- names(assays(MAE))
  if("pathwayProjection" %in% nms){
    # Remove the assay if it already exists in MAE
    MAE <- MAE[,,names(assays(MAE))[-which(names(assays(MAE))=="pathwayProjection")]]
    # Add in the new one
    MAE <- c(MAE, pathwayProjection = gsva_data, mapFrom = 1L)
  }
  else {
    # Add in the assay
    MAE <- c(MAE, pathwayProjection = gsva_data, mapFrom = 1L)
  }
  return(MAE)
}