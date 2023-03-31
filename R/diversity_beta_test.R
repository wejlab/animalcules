#' Perform a beta diversity test
#'
#' @param MAE A Multi-Assay Experiment object. Required.
#' @param tax_level The taxon level at which organisms should be grouped. Req'd.
#' @param input_beta_method Can be either "bray" or "jaccard". Required.
#' @param input_select_beta_condition Condition to group samples
#' Should be a character string of a colData column name. Required.
#' @param input_select_beta_stat_method The test to be used. Can be one of
#' either "PERMANOVA", "Wilcoxon rank sum test", or "Kruskal-Wallis". Required.
#' @param input_num_permutation_permanova The number of permutations to be used.
#' @return A plotly object.
#'
#' @examples
#' data_dir <- system.file("extdata/MAE.rds", package = "animalcules")
#' toy_data <- readRDS(data_dir)
#' p <- diversity_beta_test(toy_data,
#'   tax_level = "genus",
#'   input_beta_method = "bray",
#'   input_select_beta_condition = "DISEASE",
#'   input_select_beta_stat_method = "Wilcoxon rank sum test",
#'   input_num_permutation_permanova = 999
#' )
#' p
#'
#' @export

diversity_beta_test <- function(MAE,
    tax_level,
    input_beta_method,
    input_select_beta_condition,
    input_select_beta_stat_method,
    input_num_permutation_permanova = 999) {
    # Extract data
    microbe <- MAE[["MicrobeGenetics"]]
    # organism x taxlev
    tax_table_init <- microbe %>%
        SummarizedExperiment::rowData() %>%
        as.data.frame()
    # sample x condition
    sam_table <- microbe %>%
        SummarizedExperiment::colData() %>%
        as.data.frame()
    # organism x sample
    counts_table_init <- microbe %>%
        SummarizedExperiment::assays() %>%
        as.data.frame() %>%
        dplyr::select(all_of(rownames(sam_table)))
    
    # Sum counts by taxon level and return counts
    counts_table <- counts_table_init %>% # Sum counts by taxon level
        upsample_counts(tax_table_init, tax_level)
    
    # change tax table size
    tax_table_int <- tax_table_init %>%
        dplyr::select(seq_len(dplyr::all_of(tax_level))) %>%
        dplyr::arrange(dplyr::across(dplyr::starts_with(tax_level))) %>%
        dplyr::distinct(.keep_all = TRUE) %>%
        # Remove columns consisting of only NA's
        dplyr::select_if(~ sum(!is.na(.)) > 0)
        # factorize each column
    cols <- colnames(tax_table_int)
    tax_table <- dplyr::mutate(tax_table_int, dplyr::across(.cols = all_of(cols), .fns = factor))
    # Genera must also be in the counts_table rownames
    ind <- tax_table[, tax_level] %in% rownames(counts_table)
    tax_table <- tax_table[ind, ]
    rownames(tax_table) <- tax_table[, tax_level]
    
    # generate beta diversity
    if (input_beta_method %in% c("bray", "jaccard")) {
        # Then use vegdist from vegan to generate a bray distance object:
        dist.mat <- counts_table %>%
            t() %>%
            vegan::vegdist(method = input_beta_method) %>%
            as.matrix()
    } else {
        stop("input_beta_method not recognized.")
    }
    
    ind <- which(colnames(sam_table) == input_select_beta_condition)
    colnames(sam_table)[ind] <- "condition"
    
    if (input_select_beta_stat_method == "PERMANOVA") {
        beta.div <- vegan::adonis2(dist.mat ~ condition,
            data = sam_table,
            permutations = input_num_permutation_permanova
        )
        return(beta.div)
    } else {
        dist.within.a <- c()
        dist.within.b <- c()
        dist.between <- c()
        for (i in seq_len(nrow(dist.mat))) {
            for (j in seq_len(nrow(dist.mat))) {
                if (sam_table$condition[i] ==
                        unique(sam_table$condition)[1] & 
                        sam_table$condition[j] ==
                        unique(sam_table$condition)[1]) {
                    dist.within.a <- c(dist.within.a, dist.mat[i, j])
                } else if (sam_table$condition[i] ==
                        unique(sam_table$condition)[2] &
                        sam_table$condition[j] ==
                        unique(sam_table$condition)[2]) {
                    dist.within.b <- c(dist.within.b, dist.mat[i, j])
                } else {
                    dist.between <- c(dist.between, dist.mat[i, j])
                }
            }
        }
        
        dist.list <- list(dist.within.a, dist.within.b, dist.between) %>%
            magrittr::set_names(c(
                unique(sam_table$condition)[1],
                unique(sam_table$condition)[2],
                "between"
            ))
        if (input_select_beta_stat_method == "Wilcoxon rank sum test") {
            result.list <- list()
            group.name <- c()
            for (i in seq_len(length(dist.list))) {
                dist.list.tmp <-
                    dist.list[which(names(dist.list) != names(dist.list)[i])]
                group.name[i] <- paste(names(dist.list.tmp), collapse = " and ")
                result.list[[i]] <-
                    wilcox.test(dist.list.tmp[[1]], dist.list.tmp[[2]])
            }
            output.table <- NULL
            for (i in seq_len(length(result.list))) {
                output.tmp <-
                    c(result.list[[i]]$method, result.list[[i]]$p.value)
                output.table <- cbind(output.table, output.tmp)
            }
            rownames(output.table) <- c("Method", "P-value")
            colnames(output.table) <- group.name
            return(output.table)
        } else {
            tmp <- list(dist.within.a, dist.within.b, dist.between) %>%
                kruskal.test()
            output <- c(tmp$method, tmp$p.value) %>%
                data.frame() %>%
                magrittr::set_rownames(c("Method", "P-value"))
            return(output)
        }
    }
}
