#' Beta diversity heatmap
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param input_beta_method bray, jaccard
#' @param input_bdhm_select_conditions Which condition to group samples
#' @param input_bdhm_sort_by Sorting option e.g. "nosort", "conditions"
#' @return A plotly object
#'
#' @examples
#' data_dir = system.file("extdata/MAE.rds", package = "animalcules")
#' toy_data <- readRDS(data_dir)
#' p <- diversity_beta_heatmap(toy_data,
#'                             tax_level = "genus",
#'                             input_beta_method = "bray",
#'                             input_bdhm_select_conditions = "DISEASE",
#'                             input_bdhm_sort_by = "conditions")
#' p
#'
#' @import dplyr
#' @import plotly
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#' @export

diversity_beta_heatmap <- function(MAE,
                                    tax_level,
                                    input_beta_method,
                                    input_bdhm_select_conditions,
                                    input_bdhm_sort_by = 
                                    c("nosort", "conditions")){

    # Extract data
    microbe <- MAE[['MicrobeGenetics']] #double bracket subsetting is easier
    #host <- MAE[['HostGenetics']]
    tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- 
    as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

    # Sum counts by taxon level and return counts
    counts_table %<>%
        # Sum counts by taxon level
        upsample_counts(tax_table, tax_level)


    #Then use vegdist from vegan to generate a bray distance object:
    dist.mat <- vegan::vegdist(t(counts_table), method = input_beta_method)
    dist.mat <- as.matrix(dist.mat)
    dist.mat <- 
    dist.mat[order(match(rownames(dist.mat), 
    rev(rownames(dist.mat)))),,drop=FALSE]


    if (!is.null(input_bdhm_select_conditions)) {
        df.sam <- sam_table[,input_bdhm_select_conditions,drop=FALSE]
        if (input_bdhm_sort_by == "conditions") {
            for (i in ncol(df.sam):1) {
                df.sam <- df.sam[rev(order(df.sam[[i]])),,drop=FALSE]
            }
        dist.mat <- 
        dist.mat[order(match(rownames(dist.mat), 
            rownames(df.sam))),,drop=FALSE]
        dist.mat <- 
        dist.mat[,rev(order(match(colnames(dist.mat), 
            rownames(df.sam)))),drop=FALSE]
        } else {
            df.sam <- 
            df.sam[order(match(rownames(df.sam), 
                rownames(dist.mat))),,drop=FALSE]
        }
    }

    m <- data.matrix(dist.mat)
    hover.txt <- c()
    for (i in seq_len(ncol(dist.mat))) {
    hover.txt <- cbind(hover.txt, dist.mat[[i]])
    }
    hm.beta <- plot_ly(x = colnames(m), y = rownames(m), z = m,
                    type = "heatmap",
                    colors= "RdPu",
                    hoverinfo = "x+y+z") %>%
    layout(xaxis = list(showticklabels = FALSE, 
    title = "", ticks = "", tickangle = -45),
    yaxis = list(showticklabels = FALSE, type = 'category', ticks = ""))

    if (!is.null(input_bdhm_select_conditions)) {
        hover.txt <- c()
        for (i in seq_len(ncol(df.sam))) {
            hover.txt <- cbind(hover.txt, df.sam[[i]])
    }
    df.sam[] <- lapply(df.sam, factor)

    # Y-axis of subplot
    m <- data.matrix(df.sam)
    m.row.normalized <- apply(m, 2, function(x)(x-min(x))/(max(x)-min(x)))
    hm.sam.y <- plot_ly(x = colnames(m.row.normalized),
                        y = rownames(m.row.normalized),
                        z = m.row.normalized,
                        type = "heatmap",
                        showscale=FALSE,
                        hoverinfo = "x+y+text",
                        transpose=FALSE,
                        text=hover.txt) %>%
        layout(xaxis = list(title = "", tickangle = -45),
            yaxis = list(showticklabels = FALSE, 
            type = 'category', ticks = ""),
            orientation=TRUE)

    # X-axis of subplot
    m <- data.matrix(df.sam)
    m.row.normalized <- apply(m, 2, function(x)(x-min(x))/(max(x)-min(x)))
    m.row.normalized = t(m.row.normalized)
    m.row.normalized = 
    m.row.normalized[order(match(rownames(m.row.normalized), 
                rev(rownames(m.row.normalized)))),,drop=FALSE]
    hm.sam.x <- plot_ly(x = colnames(m.row.normalized),
                        y = rownames(m.row.normalized),
                        z = m.row.normalized,
                        type = "heatmap",
                        showscale=FALSE,
                        hoverinfo = "x+y+text",
                        transpose=FALSE,
                        text=t(hover.txt)) %>%
        layout(xaxis = list(showticklabels = FALSE, type = 'category',
                    ticks = "", autorange="reversed"),
            yaxis = list(title = "", tickangle = -45),
            orientation=TRUE)
    }

    empty <- plotly_empty(type = "scatter")

    if (!is.null(input_bdhm_select_conditions)) {
        hm.sam.beta.top <- subplot(empty, hm.sam.x, widths=c(0.1,  0.9))
        hm.sam.beta.bot <- subplot(hm.sam.y, hm.beta, widths=c(0.1,  0.9))
        hm.sam.beta <- 
        subplot(hm.sam.beta.top, 
            hm.sam.beta.bot, nrows=2, heights=c(0.1,  0.9))
        hm.sam.beta$elementId <- NULL # To suppress a shiny warning
        return(hm.sam.beta)
    } else {
        hm.beta$elementId <- NULL # To suppress a shiny warning
        return(hm.beta)
    }


}

