#' Dimensionality reduction through PCA
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param color A condition to color data points by e.g. "AGE"
#' @param shape A condition to shape data points by e.g. "SEX"
#' @param pcx Principal component on the x-axis e.g. 1
#' @param pcy Principal component on the y-axis e.g. 2
#' @param datatype Datatype to use e.g. c("counts", "relabu", "logcpm")
#' @return A list with a plotly object and summary table
#'
#' @examples
#' toy_data <- readRDS("data/MAE.rds")
#' result <- dimred_pca(toy_data,
#'                      tax_level="genus",
#'                      color="AGE",
#'                      shape="DISEASE",
#'                      pcx=1,
#'                      pcy=2,
#'                      datatype="logcpm")
#' result$plot
#' result$table
#'
#' @import dplyr
#' @import plotly
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#'
#' @export
dimred_pca <- function(MAE,
                       tax_level,
                       color,
                       shape=NULL,
                       pcx=1,
                       pcy=2,
                       datatype=c("counts", "relabu", "logcpm")) {

    # Default variables
    datatype <- match.arg(datatype)

    # Extract data
    microbe <- MultiAssayExperiment::experiments(MAE)[[1]]
    host <- MultiAssayExperiment::experiments(MAE)[[2]]
    tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

    df <- counts_table %>%
          # Sum counts by taxon level
          upsample_counts(tax_table, tax_level) %>%
          # Choose data type
          {
              if (datatype == "relabu") {
                  counts_to_relabu(.)
              } else if (datatype == "logcpm") {
                  counts_to_logcpm(.)
              } else {
                  .
              }
          } %>%
          # Fix constant/zero row
          {
              if (sum(rowSums(as.matrix(.)) == 0) > 0){
                  . <- .[-which(rowSums(as.matrix(.)) == 0),]
              } else {
                  .
              }
          } %>%
          # Transpose
          t()

    # PCA
    df.prcomp <- stats::prcomp(df, scale=T)
    # Principle Components
    df.pca <- df.prcomp$x
    # Importance
    df.imp <- t(summary(df.prcomp)$importance)

    # Merge in covariate information
    if (!is.null(shape)) {
        df.pca.m <- merge(df.pca, sam_table[, c(color, shape), drop=F], by=0, all=T)
    } else {
        df.pca.m <- merge(df.pca, sam_table[, color, drop=F], by=0, all=T)
        shape <- 'shape' # Referenced by plotly later
        df.pca.m[[shape]] <- 1 # Constant results in omitting shape
    }

    # Plotly | Scatterplot
    p <- plot_ly(df.pca.m,
                 x = as.formula(paste("~PC", pcx, sep = "")),
                 y = as.formula(paste("~PC", pcy, sep = "")),
                 mode = "markers",
                 color = as.formula(paste("~", color, sep = "")),
                 symbol = as.formula(paste("~", shape, sep = "")),
                 type = "scatter",
                 text = df.pca.m$Row.names,
                 marker = list(size = 10))

    p$p <- NULL # To suppress a shiny warning

    # Formatting importance table
    colnames(df.imp) = c("Standard Deviation",
                         "Variance Explained",
                         "Cumulative Variance")

    # Show variance as a percentage
    df.imp[,2] <- scales::percent(as.numeric(df.imp[,2]))
    df.imp[,3] <- scales::percent(as.numeric(df.imp[,3]))

    return(list(plot=p, table=df.imp))
}
