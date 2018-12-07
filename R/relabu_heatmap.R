#' Plot heatmap of sample level counts in logcpm
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param sort_by Sort bars by one of c("nosort", "conditions", "organisms")
#' @param sample_conditions Plot associatied conditions with samples e.g. c("SEX", "AGE")
#' @param isolate_organisms Isolate specific organisms e.g. c("Hepacivirus")
#' @param isolate_samples Isolate specific samples e.g. c("SAM_01", "SAM_02")
#' @param discard_samples Discard specific samples e.g. c("SAM_01", "SAM_02")
#' @param log_cpm Convert counts to logcpm
#' @return A plotly object
#'
#' @examples
#' toy_data <- readRDS("data/MAE.rds")
#' p <- relabu_heatmap(toy_data,
#'                    tax_level="genus",
#'                    sort_by="conditions",
#'                    sample_conditions=c("SEX", "AGE"))
#' p
#'
#' @import dplyr
#' @import plotly
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#'
#' @export
relabu_heatmap <- function(MAE,
                           tax_level,
                           sort_by=c("nosort", "conditions", "organisms"),
                           sample_conditions=c(),
                           isolate_organisms=c(),
                           isolate_samples=c(),
                           discard_samples=c(),
                           log_cpm=TRUE) {

    # Default variables
    sort_by <- match.arg(sort_by)

    # Extract data
    microbe <- MultiAssayExperiment::experiments(MAE)[[1]]
    host <- MultiAssayExperiment::experiments(MAE)[[2]]
    tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

    counts_table <- pick_samples(counts_table, isolate_samples, discard_samples)

    # Ensure conditions are all factored
    sam_table %<>% df_char_to_factor()

    # Subset and match sam table on the same samples as counts table
    sam_table <- sam_table %>%
        .[rownames(.) %in% colnames(counts_table),,drop=F] %>%
        .[match(rownames(.), colnames(counts_table)),,drop=F]

    # Ensure sam table has the same subset of samples and is in the correct order
    stopifnot(colnames(counts_table) == rownames(sam_table))

    # Sum counts by taxon level and return counts or logcpm(counts)
    counts_table <- counts_table %>%
                        upsample_counts(tax_table, tax_level) %>%
                        {if(log_cpm) counts_to_logcpm(.) else .} %>%
                        base::t() %>%
                        base::as.data.frame()

    # Isolate organisms if specified
    if (!is.null(isolate_organisms)) {
        counts_table <- counts_table[,isolate_organisms,drop=F]
    }

    # Reorder by most prominent organisms
    counts_table <- counts_table[,order(colSums(counts_table)),drop=F]

    # Order samples by organisms if not by conditons
    if (sort_by == "organisms") {
        for (i in 1:ncol(counts_table)) {
            counts_table <- counts_table[order(counts_table[,i]),,drop=F]
        }
    }

    # If any conditions are selected make a side bar
    if (!is.null(sample_conditions)) {
        sam_table <- sam_table[,sample_conditions,drop=F]
        if (sort_by == "conditions") {
            for (i in ncol(sam_table):1) {
                sam_table <- sam_table[order(sam_table[[i]]),,drop=F]
            }
            # Reorder stacked barplot
            counts_table <- counts_table[order(match(rownames(counts_table), rownames(sam_table))),,drop=F]
        } else {
            sam_table <- sam_table[order(match(rownames(sam_table), rownames(counts_table))),,drop=F]
        }
    }

    # Plotly | Heatmap Counts
    mat <- data.matrix(counts_table)
    hm_c <- plot_ly(x = colnames(mat),
                    y = rownames(mat),
                    z = mat,
                    type = "heatmap",
                    colors= "RdPu",
                    hoverinfo = "x+y+z") %>%
                    layout(xaxis = list(showticklabels = F,
                                        title = "",
                                        ticks = "",
                                        tickangle = -45),
                           yaxis = list(showticklabels = F,
                                        type = 'category',
                                        ticks = ""))

    # Plotly | Heatmap Samples
    if (!is.null(sample_conditions)) {

        # Retain hover-text information before conditions are factorized
        hover.txt <- c()
        for (i in 1:ncol(sam_table)) {
            hover.txt <- cbind(hover.txt, as.character(sam_table[[i]]))
        }

        # Normalized matrix
        mat <- sam_table %>%
            data.matrix() %>%
            apply(2, function(x)(x-min(x))/(max(x)-min(x)))

        # Plotly | Heatmap Samples
        hm_s <- plot_ly(x = colnames(mat),
                        y = rownames(mat),
                        z = mat,
                        type = "heatmap",
                        showscale=F,
                        hoverinfo = "x+y+text",
                        text=hover.txt) %>%
                        layout(xaxis = list(title = "",
                                            tickangle = -45),
                               yaxis = list(showticklabels = F,
                                            type = 'category',
                                            ticks = ""))
    }

    # Create a multiplot if any conditions are selected
    if (!is.null(sample_conditions)) {
        hm_c_s <- subplot(hm_s, hm_c, widths=c(0.1,  0.9))
        hm_c_s$elementId <- NULL # To suppress a shiny warning
        return(hm_c_s)
    } else {
        hm_c$elementId <- NULL # To suppress a shiny warning
        return(hm_c)
    }
}
