#' Run enrichR analysis on significantly correlated groups using hypeR
#' 
#' @param corr_results Results from corr_func. Second assay selected must contain gene expression values
#' @param group_selected User input, which group to run in Enrichr
#' @param geneset_db User input, which database to use for Enrichr analysis
#' 
#' @return Plotly displaying enrichment terms and scores
#' 
#' @examples 
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' results <- corr_func(toy_data, 
#'                      asys = c('MicrobeGenetics', 'HostGenetics'),
#'                      tax_level="genus")
#'                      
#' group <- 'Acinetobacter' # microbe from results
#' 
#' db <- 'KEGG_2019_Human' # Enrichr database
#' 
#' p <- enrich_cors(results$summary, group, db) # Run enrichment
#' p
#' 
#' @import hypeR
#' @import dplyr
#' @importFrom ggplot2 theme
#' 
#' @export

enrich_cors <- function(corr_results, group_selected, geneset_db) {
  signature <- corr_results %>%
    dplyr::filter(OTU == group_selected) %>%
    dplyr::pull(Groups)
  signature <- strsplit(signature, split = ";")[[1]]
  gs <- hypeR::enrichr_gsets(geneset_db, db = "Enrichr")
  genesets <- gs$genesets
  hyp <- hypeR::hypeR(signature, genesets, test = "hypergeometric")
  p <- hypeR::hyp_dots(hyp)
  p <- p + theme(axis.text = element_text(size = 12, face = "bold"))
  p <- plotly::ggplotly(p)
  return(p)
}
