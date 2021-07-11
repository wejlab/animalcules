#' Pathway projection using GSVA
#' 
#' @param MAE MultiAssayExperiment having at least 1 gene expression assay (containing gene symbols)
#' @param collection User-specified MSigDB collection
#' @param sub_collection Optional, user-specified MSigDB sub-collection
#' @return SummarizedExperiment containing ssGSEA scores of selected gene sets
#' 
#' @import hypeR
#' @import GSVA
#' 
#' @examples
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS("MAE.rds")
#' 
#' gsva_results <- path_proj(toy_data, "H", NA) # H = Hallmark gene sets
#' 
#' @export

path_proj <- function(MAE, collection, sub_collection) {
  if (is.na(sub_collection)) {
    # if there is no sub-collection
    gs <- hypeR::msigdb_gsets(species = "Homo sapiens", collection)
    gs <- gs$genesets
  } else {
    # if there is a sub-collection
    gs <- hypeR::msigdb_gsets(species = "Homo sapiens", collection, 
                              sub_collection)
    gs <- gs$genesets
  }
  # GSVA Analysis --> getting the ssGSEA scores from the gene set
  # groupings
  gsva_data <- gsva(MAE[["hostExpression"]], gs, method = "ssgsea")
  return(gsva_data)
}