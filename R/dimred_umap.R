#' Dimensionality reduction through UMAP
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param color A condition to color data points by e.g. "AGE"
#' @param shape A condition to shape data points by e.g. "SEX"
#' @param k Plot dimensions e.g. c("2D","3D")
#' @param n_neighbors The number of nearest neighbors
#' @param min_dist Determines how close points appear in the final layout
#' @param datatype Datatype to use e.g. c("counts", "relabu", "logcpm")
#' @return A plotly object
#'
#' @examples
#' data_dir = system.file("extdata/MAE.rds", package = "animalcules")
#' toy_data <- readRDS(data_dir)
#' p <- dimred_umap(toy_data,
#'                  tax_level="phylum",
#'                  color="AGE",
#'                  shape="GROUP",
#'                  k="3D",
#'                  n_neighbors=15,
#'                  min_dist=3,
#'                  datatype="logcpm")
#' p
#'
#' @import dplyr
#' @import plotly
#' @import umap
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#'
#' @export
dimred_umap <- function(MAE,
                        tax_level,
                        color,
                        shape=NULL,
                        k=c("2D","3D"),
                        n_neighbors=15,
                        min_dist=3,
                        datatype=c("counts", "relabu", "logcpm")) {

    # Default variables
    k <- ifelse(match.arg(k) == "2D", 2, 3)
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

    config <- umap.defaults
    config$n_components = k
    config$n_neighbors = n_neighbors
    config$min_dist = min_dist

    df.umap <- umap(df, config)$layout

    if (k == 2) {colnames(df.umap) <- c("X", "Y")} else {colnames(df.umap) <- c("X", "Y", "Z")}

    # Merge in covariate information
    if (!is.null(shape)) {
        df.umap.m <- merge(df.umap, sam_table[, c(color, shape), drop=F], by=0, all=TRUE)

        # When shape is required
        shape <- colnames(df.umap.m)[ncol(df.umap.m)] # Bypass duplicate colnames if color == shape
        df.umap.m[[shape]] <- as.factor(df.umap.m[[shape]])

    } else {
        df.umap.m <- merge(df.umap, sam_table[, color, drop=FALSE], by=0, all=TRUE)
        shape <- 'shape' # Referenced by plotly later
        df.umap.m[[shape]] <- 1 # Constant results in omitting shape
    }

    # Plotly | Scatterplot
    if (k == 2) {

        # 2D Plot
        p <- plot_ly(df.umap.m,
                     x = as.formula("~X"),
                     y = as.formula("~Y"),
                     mode = "markers",
                     color = as.formula(paste("~", color, sep = "")),
                     symbol = as.formula(paste("~", shape, sep = "")),
                     type = "scatter",
                     text = df.umap.m$Row.names,
                     marker = list(size = 10))
    } else {

        # 3D Plot
        p <- plot_ly(df.umap.m,
                     x = as.formula("~X"),
                     y = as.formula("~Y"),
                     z = as.formula("~Z"),
                     mode = "markers",
                     color = as.formula(paste("~", color, sep = "")),
                     symbol = as.formula(paste("~", shape, sep = "")),
                     symbols = c("circle", "square", "diamond", "cross", "square-open", "circle-open", "diamond-open", "x"),
                     type = "scatter3d",
                     text = df.umap.m$Row.names,
                     marker = list(size = 6))
    }

    p$p <- NULL # To suppress a shiny warning

    return(p)
}