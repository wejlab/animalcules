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

corr_network <- function(MAE, assay, cormat, group, tax_level = NA) {
  sumE <- MAE[[assay]]
  sam_table <- as.data.frame(colData(sumE)) # sample x condition
  counts_table <- as.data.frame(assays(sumE))[,rownames(sam_table)] # organism x sample
  if(!is.na(tax_level)){
    tax_table <- as.data.frame(rowData(sumE)) # organism x taxlev
    # Aggregate according to tax_level + normalize
    counts_table <- counts_table %>%
      upsample_counts(tax_table, tax_level) %>%
      counts_to_logcpm()
  } else {
    counts_table <- counts_table %>%
      counts_to_logcpm()
  }
  el <- cormat[which(rownames(cormat)==group),]
  el <- names(el[el!=0])
  counts_table <- subset(counts_table, (rownames(counts_table) %in% el)) # extracting genes from hostExpression
  #print(dim(sub_data))
  counts_table <- counts_table[rowMeans(counts_table)>=1,]
  #print("Calculating correlations...")
  datacor_s <- cor(t(counts_table), method = "spearman")
  #print("Plotting network...")
  fig <- qgraph(datacor_s, 
                graph = "cor", 
                layout = "spring", 
                vsize = 5, 
                theme = "colorblind"#,
                #threshold = "sig",
                #bonf = TRUE,
                #sampleSize=nrow(sub_data),
                #alpha=0.01
  )
  return(fig)
}
