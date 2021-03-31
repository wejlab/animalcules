#' Calculate significant correlations between microbial abundance and another assay
#'
#' @param MAE A MultiAssayExperiment object
#' @param asys List of assays to calculate correlations for. Minimum length = 1
#' @param tax_level Taxonomic level of interest. Maximum length = 2, default is set to "genus"
#' @param no.sig Minimum number of significant correlations to be extracted
#' @param correction Method for p-value correction for multiple hypotheses. Default is bonferroni
#' @param alpha If no correction is specified, will use alpha to determine significance
#'
#' @examples
#' library(SummarizedExperiment)
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' correlation_matrix <- corr_func(MAE = toy_data, asys = c(MicrobeGenetics, HostExpression), tax_level = "genus")
#'
#' @import MultiAssayExperiment
#'
#' @export

corr_func <- function(MAE, asys, tax_level = "genus", no.sig = 1, correction = "bonferroni", alpha = 0) {
  # Used to correlate microbe abundance against itself
  if (length(asys) == 1 & length(tax_level)>1) {
    # Read in the assays
    MAE_res <- rd_MAE(MAE, asys)
    subMAE <- MAE_res[[1]]
    no.samples <- MAE_res[[2]]
    asy1 <- subMAE[[asys[1]]] # assay 1
    # Grouping together samples using upsample
    if("MicrobeGenetics" %in% asys){
      tax_table <- as.data.frame(rowData(asy1)) # organism x taxlev
      sam_table <- as.data.frame(colData(asy1)) # sample x condition
      counts_table <-
        as.data.frame(assays(asy1))[,rownames(sam_table)] # organism x sample
      # Aggregate according to tax_level + normalize
      df1 <- counts_table %>%
        upsample_counts(tax_table, tax_level[1]) %>%
        counts_to_logcpm()
      df2 <- counts_table %>%
        upsample_counts(tax_table, tax_level[2]) %>%
        counts_to_logcpm()
    }
    # Getting rid of rows with 0
    df1 <- df1[rowMeans(df1)>0, ]
    df2 <- df2[rowMeans(df2)>0, ]

    # Calculating correlations + p-values
    cors_res <- calc_cors(df1, df2, no.samples)
    cors <- cors_res[[1]]
    ts <- cors_res[[2]]
    ts <- abs(ts)
    if (correction == "bonferroni") {
      alpha <- 0.05/dim(ts)[2]
      t_crit <- abs(stats::qt(alpha, no.samples-2))
      sig_cors <- calc_sig(cors, ts, no.sig, alpha)
    } else {
      sig_cors <- calc_sig(cors, ts, no.sig, alpha)
    }
    return(sig_cors)
  }

  # Using multiple assays
  if (length(asys) > 1) {
    # Read the MultiAssayExperiment
    MAE_res <- rd_MAE(MAE, asys)
    subMAE <- MAE_res[[1]]
    no.samples <- MAE_res[[2]]

    # assay 1
    asy1 <- subMAE[[asys[1]]]
    if(asys[1] == "MicrobeGenetics"){
      # Aggregate count + normalize
      tax_table <- as.data.frame(rowData(asy1)) # organism x taxlev
      sam_table1 <- as.data.frame(colData(asy1)) # sample x condition
      counts_table1 <-
        as.data.frame(assays(asy1))[,rownames(sam_table1)] # organism x sample
      df1 <- counts_table1 %>%
        upsample_counts(tax_table, tax_level) %>%
        counts_to_logcpm()
    } else {
      sam_table1 <- as.data.frame(colData(asy1)) # sample x condition
      counts_table1 <-
        as.data.frame(assays(asy1))[,rownames(sam_table1)] # organism x sample
      df1 <- counts_table1 %>%
        counts_to_logcpm()
    }
    df1 <- df1[rowMeans(df1)>0, ]

    # assay 2
    asy2 <- subMAE[[asys[2]]]
    if(asys[2] == "MicrobeGenetics"){
      tax_table <- as.data.frame(rowData(asy2)) # organism x taxlev
      sam_table2 <- as.data.frame(colData(asy2)) # sample x condition
      counts_table2 <-
        as.data.frame(assays(asy2))[,rownames(sam_table2)] # organism x sample
      df2 <- counts_table2 %>%
        upsample_counts(tax_table, tax_level) %>%
        counts_to_logcpm()
    } else{
      sam_table2 <- as.data.frame(colData(asy2)) # sample x condition
      counts_table2 <-
        as.data.frame(assays(asy2))[,rownames(sam_table2)] # organism x sample
      df2 <- counts_table2 %>%
        counts_to_logcpm()
    }
    df2 <- df2[rowMeans(df2)>0, ]  # genes with non-zero expression

    # Correlations
    cors_res <- calc_cors(df1, df2, no.samples)
    cors <- cors_res[[1]]
    ts <- cors_res[[2]]
    ts <- abs(ts)
    if (correction == "bonferroni") {
      alpha <- 0.05/dim(ts)[2]
      t_crit <- abs(stats::qt(alpha, no.samples-2))
      sig_cors <- calc_sig(cors, ts, no.sig, t_crit)
    } else {
      sig_cors <- calc_sig(cors, ts, no.sig, t_crit)
    }
    return(sig_cors)
  }
}
