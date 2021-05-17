#' Pathway projection using GSVA
#' 
#' @param MAE MultiAssayExperiment object, containing at least 1 gene expression assay
#' @param collection User-specified MSigDB collection
#' @param sub-collection Optional, user-specified MSigDB sub-collection
#' 
#' @return SummarizedExperiment object containing ssGSEA scores of selected gene sets
#' 
#' @import hypeR
#' @import GSVA
#' 
#' @export

path_proj <- function(MAE, collection, sub_collection = NA){
  if(is.na(sub_collection)){ # if there is no sub-collection
    gs <- hypeR::msigdb_gsets(species = "Homo sapiens", collection)
    gs <- gs$genesets
  } else{ # if there is a sub-collection
    gs <- hypeR::msigdb_gsets(species = "Homo sapiens", collection, sub_collection)
    gs <- gs$genesets
  }
  # GSVA Analysis --> getting the ssGSEA scores from the gene set groupings
  gsva_data <- gsva(MAE[["hostExpression"]], gs, method = "ssgsea")
  return(gsva_data)
}
