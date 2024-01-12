#' Beta diversity NMDS plot
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param input_beta_method bray, jaccard
#' @param input_select_beta_condition Which condition to group samples
#'
#' @return A plotly object/NMDS plot
#'
#' @import vegan
#' @import tibble
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @import SummarizedExperiment
#'
#' @export

diversity_beta_NMDS <- function(MAE,
    tax_level,
    input_beta_method,
    input_select_beta_condition) {
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
    sam_table["Sample.ID"] <- rownames(sam_table)
    
    #Brie's code for NMDS
    NMDS_res <- vegan::metaMDS(dist.mat, # Our community-by-species matrix 
        k=2, try = 20, trymax = 500) # The number of reduced dimensions
    
    # Large scatter around lines would indicate failure to preserve fit
    #vegan::stressplot(NMDS_res)
    
    NMDS_res_2 <- NMDS_res %>%
        magrittr::extract2("points") %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Sample.ID")
   
    cond_nmds <- sam_table %>%
        dplyr::left_join(NMDS_res_2, "Sample.ID")
    
    fit_grp <- vegan::envfit(NMDS_res, as.data.frame(sam_table$condition), perm = 999)
    
    centroids <- fit_grp$factors$centroids %>%
        as.data.frame() %>%
        tibble::rownames_to_column("condition") %>%
        tidyr::separate(condition, c(NA, "condition"), sep = ".condition") %>%
        dplyr::rename(c1 = NMDS1, c2 = NMDS2)
    
    NMDSplot <- cond_nmds %>%
        left_join(centroids, by = "condition") %>%
        ggplot(aes(x = MDS1, y = MDS2, col = `condition`)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = "Axis 1", y = "Axis 2",
            subtitle = "Bray-Curtis Dissimilarity") +
        ggplot2::ggtitle("NMDS of Beta Diversity") +
        ggforce::geom_mark_ellipse(aes(fill = condition), 
            alpha = 0.1, expand = unit(0.2, "mm")) +
        ggplot2::geom_point(aes(x = c1, y = c2),
            shape = 8, size = 3) +
        ggplot2::xlim(-0.5, 0.6) +
        ggplot2::ylim(-0.6, 0.6) +
        ggplot2::theme_classic()
 
    return (NMDSplot)
    
    }