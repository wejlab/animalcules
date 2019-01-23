#' Differential abundance analysis
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param input_da_condition Which condition is the target condition
#' @param input_da_condition_covariate Covariates added to linear function
#' @param min_num_filter Minimum number of counts of reads mapped to this microbe
#' @param input_da_padj_cutoff adjusted pvalue cutoff
#'
#' @return A plotly object
#'
#' @examples
#' toy_dat_path <- system.file("data", "MAE.rds", package = "animalcules")
#' toy_data <- readRDS(toy_dat_path)
#' p <- differential_abundance(toy_data,
#' tax_level="genus",
#' input_da_condition=c("DISEASE"),
#' input_da_condition_covariate=c("SEX"),
#' min_num_filter = 5,
#' input_da_padj_cutoff = 0.05)
#' p
#'
#' @import DESeq2
#' @import MultiAssayExperiment
#'
#' @export
differential_abundance <- function(MAE,
                                   tax_level,
                                   input_da_condition = c(),
                                   input_da_condition_covariate = NULL,
                                   min_num_filter = 5,
                                   input_da_padj_cutoff = 0.05) {


    ## tables from MAE
    microbe <- MAE[['MicrobeGenetics']] #double bracket subsetting is easier
    tax_table <- as.data.frame(SummarizedExperiment::rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(SummarizedExperiment::colData(microbe)) # sample x condition
    counts_table <- as.data.frame(SummarizedExperiment::assays(microbe))[,rownames(sam_table)] # organism x sample



    # Sum counts by taxon level
    count_table_tax <- counts_table %>%
                        upsample_counts(tax_table, tax_level)

    # sam table
    sam_table %<>% df_char_to_factor()

    # build the deseq2 formula
    if (is.null(input_da_condition_covariate)){
        dds_formula = stats::as.formula(paste("~",input_da_condition, sep = " "))
    } else{
        dds_formula = stats::as.formula(paste("~",
                                       paste(
                                           paste(input_da_condition_covariate,
                                                 collapse = " + "),
                                           input_da_condition,
                                           sep = " + "),
                                       sep = " "))
    }

    # run DEseq2
    dds <- DESeq2::DESeqDataSetFromMatrix(countData = count_table_tax,
                                          colData = sam_table,
                                          design = dds_formula)
    dds <- DESeq2::DESeq(dds)


    # filter microbes with less than min_num_filter
    keep <- base::rowSums(DESeq2::counts(dds)) >= min_num_filter
    dds <- dds[keep,]

    res <- DESeq2::results(dds)

    # reorder the result
    res = res[base::order(res$padj, na.last=NA), ]


    # reformat for reporting
    if (nrow(res) != 0){
      sigtab = res[(res$padj < input_da_padj_cutoff), ]
      if (nrow(sigtab) == 0){
        as.matrix("No differentially abundant items found!")
      } else{
        sigtab = as(sigtab, "data.frame")
        sigtab$padj <- as.numeric(formatC(sigtab$padj, format = "e", digits = 2))
        sigtab$log2FoldChange <- as.numeric(formatC(sigtab$log2FoldChange, format = "e", digits = 2))
        sigtab$microbe <- rownames(sigtab)
        rownames(sigtab) <- 1:nrow(sigtab)
        sigtab %<>% select(microbe, padj, log2FoldChange)


        num.1 <- c()
        num.2 <- c()
        # transform label into 1 and 0
        label.vec.num = as.character((sam_table %>% select(input_da_condition))[,1])
        label.vec.save <- unique(label.vec.num)
        label.vec.num[label.vec.num == unique(label.vec.num)[1]] <- 1
        label.vec.num[label.vec.num != 1] <- 0
        label.vec.num <- as.numeric(label.vec.num)
        for (i in 1:nrow(sigtab)){
          species.index <- which(rownames(count_table_tax) == sigtab[i,1])
          num.1 <- c(num.1, sum((count_table_tax[species.index,which(label.vec.num == 1)] > 0)))
          num.2 <- c(num.2, sum((count_table_tax[species.index,which(label.vec.num == 0)] > 0)))
        }

        sigtab <- cbind(sigtab, num.1)
        sigtab <- cbind(sigtab, num.2)


        df.output.prevalence <- percent(round((num.1 + num.2)/ncol(count_table_tax),4))
        sigtab <- cbind(sigtab, df.output.prevalence)


        colnames(sigtab)[ncol(sigtab)-2] <- label.vec.save[1]
        colnames(sigtab)[ncol(sigtab)-1] <- label.vec.save[2]
        colnames(sigtab)[ncol(sigtab)] <- "prevalence"


        foldChange <- c()
        for (i in 1:nrow(sigtab)){
        foldChange[i] <- round((max(as.numeric(c((sigtab[i,5] / sum(label.vec.num == 0)),
                                                 (sigtab[i,4] / sum(label.vec.num == 1))))) /
                       min(as.numeric(c((sigtab[i,5] / sum(label.vec.num == 0)),
                                        (sigtab[i,4] / sum(label.vec.num == 1)))))), digits = 2)
        }
        sigtab <- cbind(sigtab, foldChange)
        colnames(sigtab)[ncol(sigtab)] <- "Group Size adjusted fold change"
        return(sigtab)

      }

    }else{
      return(as.matrix("No differentially abundant items found!"))
    }

}
