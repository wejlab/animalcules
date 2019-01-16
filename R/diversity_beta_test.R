#' Beta diversity test (by default we use bray-curtis distance)
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param input_select_beta_condition Which condition to group samples
#' @param input_select_beta_stat_method "PERMANOVA", "Kruskal-Wallis", "Mann-Whitney"
#' @param input_num_permutation_permanova number of permutations
#' @return A plotly object
#'
#' @examples
#' toy_data <- readRDS("data/MAE.rds")
#' p <- diversity_beta_test(toy_data,
#'                          tax_level = "genus",
#'                          input_select_beta_condition = "DISEASE",
#'                          input_select_beta_stat_method = "PERMANOVA",
#'                          input_num_permutation_permanova = 999)
#' p
#'
#' @import dplyr
#' @import plotly
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#' @export

diversity_beta_test <- function(MAE,
                                tax_level,
                                input_select_beta_condition,
                                input_select_beta_stat_method,
                                input_num_permutation_permanova = 999){

    # Extract data
    microbe <- MAE[['MicrobeGenetics']] #double bracket subsetting is easier
    host <- MAE[['HostGenetics']]
    tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample



    # Sum counts by taxon level and return counts
    counts_table %<>%
          # Sum counts by taxon level
          upsample_counts(tax_table, tax_level)

    #Then use vegdist from vegan to generate a bray distance object:
    dist.mat <- vegan::vegdist(t(counts_table), method = "bray")
    dist.mat <- as.matrix(dist.mat)


    colnames(sam_table)[which(colnames(sam_table) == input_select_beta_condition)] <- "condition"

    if (input_select_beta_stat_method == "PERMANOVA"){
    # bioconductor not allow set seed within R code
    # set.seed(99)
    beta.div <- vegan::adonis2(dist.mat~condition,
                        data=sam_table,
                        permutations = input_num_permutation_permanova,
                        strata="PLOT")
    beta.div
    } else {
    dist.within.a <- c()
    dist.within.b <- c()
    dist.between <- c()
    for (i in 1:nrow(dist.mat)){
      for (j in 1:nrow(dist.mat)) {
        if (sam_table$condition[i] == unique(sam_table$condition)[1] &
            sam_table$condition[j] == unique(sam_table$condition)[1]){
          dist.within.a <- c(dist.within.a, dist.mat[i,j])
        } else if (sam_table$condition[i] == unique(sam_table$condition)[2] &
                   sam_table$condition[j] == unique(sam_table$condition)[2]){
          dist.within.b <- c(dist.within.b, dist.mat[i,j])
        } else{
          dist.between <- c(dist.between, dist.mat[i,j])
        }

      }
    }
    dist.list <- list(dist.within.a, dist.within.b, dist.between)
    names(dist.list) <- c(unique(sam_table$condition)[1], unique(sam_table$condition)[2], "between")

    if (input_select_beta_stat_method == "Mann-Whitney"){
      result.list <- list()
      group.name <- c()
      for (i in 1:length(dist.list)){
        dist.list.tmp <- dist.list[which(names(dist.list) != names(dist.list)[i])]

        group.name[i] <- paste(names(dist.list.tmp), collapse = " and ")
        result.list[[i]] <- wilcox.test(dist.list.tmp[[1]], dist.list.tmp[[2]])
      }
      output.table <- NULL
      for (i in 1:length(result.list)){
        output.tmp <- c(result.list[[i]]$method, result.list[[i]]$p.value)
        output.table <- cbind(output.table, output.tmp)
      }
      rownames(output.table) <- c("Method", "P-value")
      colnames(output.table) <- group.name
      output.table
    } else {
      tmp <- kruskal.test(list(dist.within.a, dist.within.b, dist.between))
      output <- c(tmp$method, tmp$p.value)
      output.table <- data.frame(output)
      rownames(output.table) <- c("Method", "P-value")
      output.table
    }
    }
}


