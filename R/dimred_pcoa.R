#' Dimensionality reduction through PCoA
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param color A condition to color data points by e.g. "AGE"
#' @param shape A condition to shape data points by e.g. "SEX"
#' @param axx Principle coordinate on the x-axis e.g. 1
#' @param axy Principle coordinate on the y-axis e.g. 2
#' @param axz Principle coordinate on the z-axis e.g. 2
#' @param method Method to use e.g. c("bray")
#' @return A list with a plotly object and summary table
#'
#' @examples
#' data_dir = system.file("extdata/MAE.rds", package = "animalcules")
#' toy_data <- readRDS(data_dir)
#' result <- dimred_pcoa(MAE,
#'                      tax_level="genus",
#'                      color="AGE",
#'                      shape="DISEASE",
#'                      axx=1,
#'                      axy=2,
#'                      method="bray")
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
dimred_pcoa <- function(MAE,
                        tax_level,
                        color,
                        shape=NULL,
                        axx=1,
                        axy=2,
                        axz=NULL,
                        method=c("bray")) {

    # Default variables
    method <- match.arg(method)

    # Extract data
    microbe <- MultiAssayExperiment::experiments(MAE)[[1]]
    #host <- MultiAssayExperiment::experiments(MAE)[[2]]
    tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

    # Sum counts by taxon level and return counts
    df <- counts_table %>%
          # Sum counts by taxon level
          upsample_counts(tax_table, tax_level) %>%
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

    # Distance metrics
    if (method == "bray") {
        df.dist <- vegan::vegdist(df, method="bray")
    }

    # PCoA
    df.apcoa <- ape::pcoa(df.dist)
    # Principle Coordinates
    df.pcoa <- df.apcoa$vectors
    # Importance
    df.imp <- df.apcoa$values

    # Merge in covariate information
    if (!is.null(shape)) {
        df.pcoa.m <- merge(df.pcoa, sam_table[, c(color, shape), drop=FALSE], by=0, all=TRUE)

        # When shape is required
        shape <- colnames(df.pcoa.m)[ncol(df.pcoa.m)] # Bypass duplicate colnames if color == shape
        df.pcoa.m[[shape]] <- as.factor(df.pcoa.m[[shape]])

    } else {
        df.pcoa.m <- merge(df.pcoa, sam_table[, color, drop=FALSE], by=0, all=TRUE)
        shape <- 'shape' # Referenced by plotly later
        df.pcoa.m[[shape]] <- 1 # Constant results in omitting shape
    }

    # Plotly | Scatterplot
    if (is.null(axz)) {

        # 2D Plot
        p <- plot_ly(df.pcoa.m,
                     x = as.formula(paste("~Axis.", axx, sep = "")),
                     y = as.formula(paste("~Axis.", axy, sep = "")),
                     mode = "markers",
                     color = as.formula(paste("~", color, sep = "")),
                     symbol = as.formula(paste("~", shape, sep = "")),
                     type = "scatter",
                     text = df.pcoa.m$Row.names,
                     marker = list(size = 10))
    } else {

        # 3D Plot
        p <- plot_ly(df.pcoa.m,
                     x = as.formula(paste("~Axis.", axx, sep = "")),
                     y = as.formula(paste("~Axis.", axy, sep = "")),
                     z = as.formula(paste("~Axis.", axz, sep = "")),
                     mode = "markers",
                     color = as.formula(paste("~", color, sep = "")),
                     symbol = as.formula(paste("~", shape, sep = "")),
                     symbols = c("circle", "square", "diamond", "cross", "square-open", "circle-open", "diamond-open", "x"),
                     type = "scatter3d",
                     text = df.pcoa.m$Row.names,
                     marker = list(size = 6))
    }

    p$p <- NULL # To suppress a shiny warning

    # Formatting importance table
    df.imp <- df.imp[,c(1,3,5)]
    rownames(df.imp) <- paste("Axis", 1:nrow(df.imp), sep = ".")
    colnames(df.imp) <- c("Eigenvalue",
                          "Variance Explained",
                          "Cumulative Variance")

    # Show variance as a percentage
    df.imp[,2] <- scales::percent(as.numeric(df.imp[,2]))
    df.imp[,3] <- scales::percent(as.numeric(df.imp[,3]))

    # Reorder
    df.imp <- as.data.frame(df.imp)
    df.imp$Axis <- rownames(df.imp)
    df.imp <- df.imp[,c(4,1,2,3)]

    return(list(plot=p, table=df.imp))
}
