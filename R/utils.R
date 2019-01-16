#' Upsample a counts table to a higher taxon level
#'
#' @param counts_table A organism x sample data frame of counts
#' @param tax_table A organism x taxlev data frame of labels
#' @param higher_level Higher taxon level to upsample to
#' @return A organism x sample data frame of counts aggregated by a higher taxon level
#'
#' @examples
#' toy_data <- readRDS("inst/extdata/MAE.rds")
#' microbe <- toy_data[['MicrobeGenetics']] #double bracket subsetting is easier
#' counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample
#' tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
#' counts <- upsample_counts(counts_table, tax_table, "phylum")
#'
#' @import magrittr
#' @import reshape2
#'
#' @export
upsample_counts <- function(counts_table, tax_table, higher_level) {
    counts_table$higher_level = tax_table[[higher_level]]
    counts_table <- reshape2::melt(counts_table, id.vars="higher_level") %>%
                    S4Vectors::aggregate(.~variable+higher_level, . , sum) %>%
                    reshape2::dcast(higher_level~variable) %>%
                    as.data.frame()
    rownames(counts_table) <- counts_table$higher_level
    counts_table$higher_level <- NULL
    return(counts_table)
}

#' Covert a counts table to a relative abundances table
#'
#' @param counts_table A organism x sample data frame of counts
#' @return A organism x sample data frame of relative abundances
#'
#' @examples
#' toy_data <- readRDS("inst/extdata/MAE.rds")
#' microbe <- toy_data[['MicrobeGenetics']] #double bracket subsetting is easier
#' counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample
#' relabu <- counts_to_relabu(counts_table)
#'
#' @import magrittr
#'
#' @export
counts_to_relabu <- function(counts_table) {
    sapply(counts_table, prop.table) %>%
    as.data.frame() %>%
    magrittr::set_colnames(colnames(counts_table)) %>%
    magrittr::set_rownames(rownames(counts_table))
}

#' Covert a counts table to a relative abundances table
#'
#' @param counts_table A organism x sample data frame of counts
#' @return A organism x sample data frame of logcpm counts
#'
#' @examples
#' toy_data <- readRDS("inst/extdata/MAE.rds")
#' microbe <- toy_data[['MicrobeGenetics']] #double bracket subsetting is easier
#' counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample
#' logcpm <- counts_to_logcpm(counts_table)
#'
#' @import magrittr
#'
#' @export
counts_to_logcpm <- function(counts_table) {
    sapply(counts_table, function(x) log10(x*1e6/sum(x) + 1)) %>%
    as.data.frame() %>%
    magrittr::set_colnames(colnames(counts_table)) %>%
    magrittr::set_rownames(rownames(counts_table))
}

#' Modify samples of multi-assay experiment object
#'
#' @param MAE A multi-assay experiment object
#' @param isolate_samples Isolate specific samples e.g. c("SAM_01", "SAM_02")
#' @param discard_samples Discard specific samples e.g. c("SAM_01", "SAM_02")
#' @return A multi-assay experiment object
#'
#' @examples
#' toy_data <- readRDS("inst/extdata/MAE.rds")
#' subset <- mae_pick_samples(toy_data, isolate_samples=c("subject_9", "subject_14"))
#'
#' @import MultiAssayExperiment
#'
#' @export
mae_pick_samples <- function(MAE, isolate_samples=NULL, discard_samples=NULL) {
    # Isolate all of these samples
    if (!is.null(isolate_samples)) {
        MAE <- MAE[,isolate_samples,]
    }
    # Discard all of these samples
    if (!is.null(discard_samples)) {
        id = rownames(colData(MAE))
        id_isolate = id[!id %in% discard_samples]
        MAE <- MAE[,id_isolate,]
    }
    return(MAE)
}

#' Modify organisms of multi-assay experiment object
#'
#' @param MAE A multi-assay experiment object
#' @param isolate_organisms Isolate specific organisms e.g. c("ti|001", "ti|002")
#' @param discard_organisms Discard specific organisms e.g. c("ti|001", "ti|002")
#' @return A multi-assay experiment object
#'
#' @examples
#' toy_data <- readRDS("inst/extdata/MAE.rds")
#' subset <- mae_pick_organisms(toy_data, isolate_organisms=c("ti|001", "ti|002"))
#'
#' @import MultiAssayExperiment
#'
#' @export
mae_pick_organisms <- function(MAE, isolate_organisms=NULL, discard_organisms=NULL) {
    # Isolate all of these organisms
    if (!is.null(isolate_organisms)) {
        MAE <- MAE[isolate_organisms,,]
    }
    # Discard all of these organisms
    if (!is.null(discard_organisms)) {
        microbe <- MAE[['MicrobeGenetics']]
        id = rownames(as.data.frame(assays(microbe)))
        id_isolate = id[!id %in% discard_organisms]
        MAE <- MAE[id_isolate,,]
    }
    return(MAE)
}

#' Factorize all categorical columns
#'
#' @param df A sample x condition data frame
#' @return A sample x condition data frame
#'
#' @examples
#' toy_data <- readRDS("inst/extdata/MAE.rds")
#' microbe <- toy_data[['MicrobeGenetics']] #double bracket subsetting is easier
#' sam_table <- as.data.frame(colData(microbe)) # sample x condition
#' samples <- df_char_to_factor(sam_table)
#'
#'
#' @export
df_char_to_factor <- function(df) {
    for (i in 1:ncol(df)){
        if (typeof(df[,i,drop=FALSE]) == "character"){
            df[,i] <- as.factor(df[,i])
        }
    }
    return(df)
}

#' Format decimals to percentages
#'
#' @param x An array of decimals
#' @param digits number of digits
#' @param format f
#' @return An array of formatted strings
#'
#' @examples
#' nums <- c(0.42, 0.15, 0.4, 0.563, 0.2)
#' percent(nums)
#'
#' @export
percent <- function(x, digits = 2, format = "f") {
    paste0(formatC(100 * x, format = format, digits = digits), "%")
}

#' Check if object is categorical
#'
#' @param v A single value
#' @return Boolean
#'
#' @examples
#' nums <- 2
#' is.categorical(nums)
#'
#' @export
is.categorical <- function(v) {
   if (is.integer(v) || is.numeric(v)) {
     return(FALSE)
   } else {
     return(TRUE)
   }
}

#' check if integer(0)
#'
#' @param x A single value
#' @return Boolean
#'
#' @examples
#' nums <- 2
#' is.integer0(nums)
#'
#' @export
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

#' Converts decimal percentage to string with specified digits
#'
#' @param v A single value
#' @param digits number of digits
#' @return Boolean
#'
#' @examples
#' nums <- 0.23
#' pct2str(nums)
#'
#' @export
pct2str <- function(v, digits=2) {sprintf(paste0('%.',digits,'f'), v*100)}
