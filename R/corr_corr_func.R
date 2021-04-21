#' Calculate significant correlations between microbial abundance and another assay
#'
#' @param MAE A MultiAssayExperiment object
#' @param asys List of assays to calculate correlations for. Minimum length = 1
#' @param tax_level Taxonomic level of interest. Maximum length = 2, default is set to "genus"
#' @param no.sig Minimum number of significant correlations to be extracted
#' @param correction Method for p-value correction for multiple hypotheses. Default is bonferroni
#' @param alpha If no correction is specified, will use alpha to determine significance
#'
#' @return a list with plotly heatmap, summary tables, and correlation matrix
#' 
#' @examples
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' results <- corr_func(MAE = toy_data, asys = c("MicrobeGenetics", "hostExpression"))
#' results$plot # heatmap of significant correlations
#' result$summary # summary of the significantly correlated groups
#' results$summary_t10 # top 10 groups (by size)
#' results$cormat # correlation matrix
#'
#' @import MultiAssayExperiment
#' @import heatmaply
#' @import ggplot2
#' @import dplyr
#' @import zeallot
#'
#' @export

corr_func <- function(MAE, 
                      asys, 
                      tax_level=NA, 
                      no.sig=1,
                      correction="bonferroni",
                      hide_ax=NA) {
  
  # Used to correlate microbe abundance against itself
  if (length(asys) == 1 & length(tax_level)>1) {
    #print("Only 1 asssay")
    # Read in the assays
    c(subMAE, no.samples) %<-% rd_MAE(MAE, asys)
    asy1 <- subMAE[[asys[1]]] # assay 1
    # Grouping together samples using upsample
    if("MicrobeGenetics" %in% asys){
      tax_table <- as.data.frame(rowData(asy1)) # organism x taxlev
      sam_table <- as.data.frame(colData(asy1)) # sample x condition
      df <- as.data.frame(assays(asy1))[,rownames(sam_table)] # organism x sample
      # Aggregate according to tax_level + normalize
      df1 <- df %>%
        upsample_counts(tax_table, tax_level[1]) %>%
        counts_to_logcpm()
      df2 <- df %>%
        upsample_counts(tax_table, tax_level[2]) %>%
        counts_to_logcpm()
    }
    #print("Getting dfs")
    # Getting rid of rows with 0
    df1 <- df1[rowMeans(df1)>0, ]
    df2 <- df2[rowMeans(df2)>0, ]
    
    # Calculating correlations + p-values
    #print("Calculating correlations")
    c(cors, ts) %<-% calc_cors(df1, df2, no.samples)
    ts <- abs(ts)
    print(correction)
    if(correction == "bonferroni") {
      alpha <- 0.05/dim(ts)[2]
      t_crit <- abs(stats::qt(alpha, no.samples-2))
      sig_cors <- calc_sig(cors, ts, no.sig, alpha)
    } else {
      sig_cors <- calc_sig(cors, ts, no.sig, alpha)
    }
  }
  
  # Using multiple assays
  if (length(asys) > 1) {
    #print("more than 1 assay")
    # Read the MultiAssayExperiment
    c(subMAE, no.samples) %<-% rd_MAE(MAE, asys)
    
    # assay 1
    asy1 <- subMAE[[asys[1]]]
    if(asys[1] == "MicrobeGenetics" & length(tax_level)==1){
      # Aggregate count + normalize
      tax_table <- as.data.frame(rowData(asy1)) # organism x taxlev
      sam_table1 <- as.data.frame(colData(asy1)) # sample x condition
      df1 <- as.data.frame(assays(asy1))[,rownames(sam_table1)] # organism x sample
      df1 <- df1 %>%
        upsample_counts(tax_table, tax_level) %>%
        counts_to_logcpm()
    } else {
      sam_table1 <- as.data.frame(colData(asy1)) # sample x condition
      df1 <-
        as.data.frame(assays(asy1))[,rownames(sam_table1)] # organism x sample
      df1 <- df1 %>%
        counts_to_logcpm()
    }
    df1 <- df1[rowMeans(df1)>0, ]
    
    # assay 2
    asy2 <- subMAE[[asys[2]]]
    if(asys[2] == "MicrobeGenetics" & length(tax_level)==1){
      tax_table <- as.data.frame(rowData(asy2)) # organism x taxlev
      sam_table2 <- as.data.frame(colData(asy2)) # sample x condition
      df2 <- as.data.frame(assays(asy2))[,rownames(sam_table2)] # organism x sample
      df2 <- df2 %>%
        upsample_counts(tax_table, tax_level) %>%
        counts_to_logcpm()
    } else{
      sam_table2 <- as.data.frame(colData(asy2)) # sample x condition
      df2 <- as.data.frame(assays(asy2))[,rownames(sam_table2)] # organism x sample
      df2 <- df2 %>%
        counts_to_logcpm()
    }
    df2 <- df2[rowMeans(df2)>0, ]  # genes with non-zero expression
    
    # Correlations
    #print("Calculating correlations")
    c(cors, ts) %<-% calc_cors(df1, df2, no.samples)
    ts <- abs(ts)
    if (correction == "bonferroni") {
      #print("Finding significant cors")
      alpha <- 0.05/dim(ts)[2]
      t_crit <- abs(stats::qt(alpha, no.samples-2))
      sig_cors <- calc_sig(cors, ts, no.sig, t_crit)
    } else {
      sig_cors <- calc_sig(cors, ts, no.sig, t_crit)
    }
  }
  
  # Summary Table (lists the top 10% of groups, by size)
  #print("getting summary table")
  s <- summary_cors(sig_cors)
  s10 <- s[order(s$Group_Size, decreasing = TRUE), ]
  s10 <- s10[1:round(0.1*nrow(s)),]
  
  # Plotting Heat map
  #print("Plotting heatmap")
  p <- heatmap_cors(sig_cors, hide_ax)
  return(list(plot=p, summary=s, summary_t10=s10, cormat=sig_cors))
}