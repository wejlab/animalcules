#' Calculate significant correlations two assays
#'
#' @param MAE A MultiAssayExperiment object
#' @param asys List of assays to calculate correlations for. Minimum length = 1
#' @param tax_level Taxonomic level of interest. Maximum length = 2.
#' @param no.sig Minimum number of significant correlations
#' @param correction Method for p-value correction for multiple hypotheses, selecting from one of `stats::p.value.methods`. Default is 'bonferroni'.
#' @param alpha Significance threshold. Default is 0.05
#'
#' @return a list with summary tables and correlation matrix
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
#' results$summary # summary of the significantly correlated groups
#' results$cormat # correlation matrix
#'
#' @import MultiAssayExperiment
#' @import dplyr
#' @import zeallot
#'
#' @export

corr_func <- function(MAE, asys, tax_level = NA, no.sig = 1, correction = "bonferroni", 
                      alpha = 0.05) {
  
  # Used to correlate microbe abundance against itself
  if (length(asys) == 1 & length(tax_level) > 1) {
    # print('Only 1 asssay') Read in the assays
    c(subMAE, no.samples) %<-% rd_MAE(MAE, asys)
    asy1 <- subMAE[[asys[1]]]  # assay 1
    # Grouping together samples using upsample
    if ("MicrobeGenetics" %in% asys) {
      tax_table <- as.data.frame(rowData(asy1))  # organism x taxlev
      sam_table <- as.data.frame(colData(asy1))  # sample x condition
      df <- as.data.frame(assays(asy1))[, rownames(sam_table)]  # organism x sample
      df <- df[!is.na(rowSums(df)), ]
      # Aggregate according to tax_level + normalize
      df1 <- df %>%
        upsample_counts(tax_table, tax_level[1]) %>%
        counts_to_logcpm()
      df2 <- df %>%
        upsample_counts(tax_table, tax_level[2]) %>%
        counts_to_logcpm()
    }
    # print('Getting dfs') Getting rid of rows with 0
    df1 <- df1[rowMeans(df1) >= 1, ]
    df2 <- df2[rowMeans(df2) >= 1, ]
    
    # Calculating correlations + p-values print('Calculating correlations')
    c(cors, ps) %<-% calc_cors(df1, df2, no.samples)
    # print('performing signifcance tests...')
    sig_cors <- calc_sig(cors, ps, adjust = correction, no.sig, alpha)
  }
  
  # Using multiple assays
  if (length(asys) > 1) {
    # print('more than 1 assay') Read the MultiAssayExperiment
    c(subMAE, no.samples) %<-% rd_MAE(MAE, asys)
    
    # assay 1
    asy1 <- subMAE[[asys[1]]]
    if (asys[1] == "MicrobeGenetics" & length(tax_level) == 1) {
      # Aggregate count + normalize
      tax_table <- as.data.frame(rowData(asy1))  # organism x taxlev
      sam_table1 <- as.data.frame(colData(asy1))  # sample x condition
      df1 <- as.data.frame(assays(asy1))[, rownames(sam_table1)]  # organism x sample
      df1 <- df1[!is.na(rowSums(df1)), ]
      df1 <- df1 %>%
        upsample_counts(tax_table, tax_level) %>%
        counts_to_logcpm()
    } else {
      sam_table1 <- as.data.frame(colData(asy1))  # sample x condition
      df1 <- as.data.frame(assays(asy1))[, rownames(sam_table1)]  # organism x sample
      df1 <- df1[!is.na(rowSums(df1)), ]
      df1 <- df1 %>%
        counts_to_logcpm()
    }
    df1 <- df1[rowMeans(df1) >= 1, ]
    
    # assay 2
    asy2 <- subMAE[[asys[2]]]
    if (asys[2] == "MicrobeGenetics" & length(tax_level) == 1) {
      tax_table <- as.data.frame(rowData(asy2))  # organism x taxlev
      sam_table2 <- as.data.frame(colData(asy2))  # sample x condition
      df2 <- as.data.frame(assays(asy2))[, rownames(sam_table2)]  # organism x sample
      df2 <- df2[!is.na(rowSums(df2)), ]
      df2 <- df2 %>%
        upsample_counts(tax_table, tax_level) %>%
        counts_to_logcpm()
    } else {
      sam_table2 <- as.data.frame(colData(asy2))  # sample x condition
      df2 <- as.data.frame(assays(asy2))[, rownames(sam_table2)]  # organism x sample
      df2 <- df2[!is.na(rowSums(df2)), ]
      df2 <- df2 %>%
        counts_to_logcpm()
    }
    df2 <- df2[rowMeans(df2) >= 1, ]  # genes with non-zero expression
    
    # Correlations print('Calculating correlations...')
    c(cors, ps) %<-% calc_cors(df1, df2, no.samples)
    # print('performing signifcance tests...')
    sig_cors <- calc_sig(cors, ps, adjust = correction, no.sig, alpha)
  }
  
  # Summary Table (lists the top 10% of groups, by size) 
  #print('getting summary table...')
  s <- summary_cors(sig_cors)
  return(list(summary = s, cormat = sig_cors))
}
