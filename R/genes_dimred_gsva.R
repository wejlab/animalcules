#' Produces GSVA scores using gene sets generated from user-selected MSigDB pathways
#' 
#' @param MAE MultiAssayExperiment containing a "hostExpression" SummarizedExperiment object
#' @param collection MSigDB collection of gene sets
#' @param sub_collection MSigDB sub-collection. Only available for some collections
#' @return A SummarizedExperiment object containing the ssGSEA scores of hostExpression

genes_dimred_gsva <- function(MAE, collection, sub_collection = NA){
  msig_df <- msigdbr::msigdbr(species = "Homo sapiens",
                              category = collection)
  g <- c()
  gene_set <- list()
  if(is.na(sub_collection)){ # if there is no sub-collection
    msig_df <- msig_df %>% 
      dplyr::select(c(gs_cat, gs_name, human_gene_symbol))
    msig_df$gs_cat <- factor(msig_df$gs_cat)
    msig_df$gs_name <- factor(msig_df$gs_name)
    for(pathway in levels(msig_df$gs_name)){
      g <- c(msig_df[which(msig_df$gs_name==pathway),3])[[1]]
      gene_set <- c(gene_set, list(g))
    }
    names(gene_set) <- levels(msig_df$gs_name)
  } else{ # if there is a sub-collection
    msig_df <- msig_df %>% 
      dplyr::filter(gs_subcat == sub_collection) # Filtering by sub-collection (only necessary for some collections)
    msig_df <- msig_df %>% 
      dplyr::select(c(gs_cat, gs_subcat, gs_name, human_gene_symbol))
    msig_df$gs_subcat <- factor(msig_df$gs_subcat)
    msig_df$gs_cat <- factor(msig_df$gs_cat)
    msig_df$gs_name <- factor(msig_df$gs_name)
    for(pathway in levels(msig_df$gs_name)){
      g <- c(msig_df[which(msig_df$gs_name==pathway),4])[[1]] # extra column added in if there is a sub-collection
      gene_set <- c(gene_set, list(g))
    }
    names(gene_set) <- levels(msig_df$gs_name)
  }
  # GSVA Analysis --> getting the ssGSEA scores from the gene set groupings
  gsva_data <- GSVA::gsva(MAE[["hostExpression"]], gene_set, method = "ssgsea")
  return(gsva_data)
}
