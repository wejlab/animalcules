#' Create a co-occurence network, given a counts table and correlation matrix
#' 
#' @param MAE A MultiAssayExperiment object
#' @param assay SummarizedExperiment of interest, within the MultiAssayExperiment
#' @param cormat Correlation matrix produced by corr_func
#' @param group Correlation group to produce network for
#' @return qgraph network of the co-occurences within the group selected
#' 
#' @examples 
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' results <- corr_func(toy_data, 
#'                      asys = c('MicrobeGenetics', 'hostExpression'),
#'                      tax_level="genus")
#' group <- 'Actinomyces' # microbe from results$summary
#' fig <- corr_network(MAE = toy_data, 
#'                     assay = "hostExpression", 
#'                     cormat = results$cormat, 
#'                     group = group)
#' fig
#' 
#' @import qgraph
#' 
#' @export

corr_network <- function(MAE, assay, cormat, group) {
  counts_table <- assay(MAE[[assay]])
  el <- cormat[which(rownames(cormat)==group),]
  el <- names(el[el!=0])
  sub_data <- subset(counts_table, (rownames(counts_table) %in% el)) # extracting genes from hostExpression
  sub_data <- counts_to_logcpm(sub_data)
  sub_data <- sub_data[rowMeans(sub_data)>=1,]
  datacor_s <- stats::cor(t(sub_data), method = "spearman")
  fig <- qgraph::qgraph(datacor_s, 
                        graph = "cor", 
                        layout = "spring", 
                        vsize = 3, 
                        theme = "colorblind"
  )
  return(fig)
}