#' Dimensionality reduction through t-SNE, for gene expression assays
#'
#' @param MAE A multi-assay experiment object
#' @param color A condition to color data points by e.g. "AGE"
#' @param shape A condition to shape data points by e.g. "SEX"
#' @param k Plot dimensions e.g. c("2D","3D")
#' @param initial_dims The number of dimensions to use in reduction method
#' @param perplexity Optimal number of neighbors
#' @param datatype Datatype to use e.g. c("logcpm", "counts")
#' @param tsne_cache Pass the cached data back into the function
#' @return A list with a plotly object and cached data
#'
#' @examples
#' data_dir = system.file("extdata/MAE.rds", package = "animalcules")
#' toy_data <- readRDS(data_dir)
#' results <- dimredg_tsne(toy_data,
#'                        color="AGE",
#'                        shape="GROUP",
#'                        k="3D",
#'                        initial_dims=30,
#'                        perplexity=10,
#'                        datatype="logcpm")
#' results$plot
#'
#' @import dplyr
#' @import plotly
#' @import tsne
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#'
#' @export

dimredg_tsne <- function(MAE,
                         color,
                         shape=NULL,
                         k=c("2D","3D"),
                         initial_dims=30,
                         perplexity=10,
                         datatype=c("logcpm", "counts"),
                         tsne_cache=NULL) {
  
  # Extract data
  host <- MAE[['hostExpression']]
  sam_table <- as.data.frame(colData(host)) # sample x condition
  counts_table <- 
    as.data.frame(assays(host))[,rownames(sam_table)] # organism x sample
  if (is.null(tsne_cache)) {
    # Default variables
    k <- ifelse(match.arg(k) == "2D", 2, 3)
    datatype <- match.arg(datatype)
    df <- counts_table %>%
      {
        # Choose data type
        if (datatype == "logcpm") {
          counts_to_logcpm(.)
        } else {
          .
        }
      } %>%
      # Fix constant/zero row
      {
        if (sum(base::rowSums(as.matrix(.)) == 0) > 0){
          . <- .[-which(base::rowSums(as.matrix(.)) == 0),]
          #. <- .[which(base::rowSums(as.matrix(.)) >= 1),]
        } else {
          .
        }
      } %>%
      # Transpose
      t()
    
    # PCA
    df.prcomp <- stats::prcomp(df, scale=TRUE)
    # Principle Components
    df.pca <- df.prcomp$x
    
    # t-SNE
    df.tsne <- 
      tsne(scale(df.pca), k=k, initial_dims=initial_dims, perplexity=perplexity)
    rownames(df.tsne) <- rownames(df.pca)
    if (k == 2) {colnames(df.tsne) <- 
      c("X", "Y")} else {colnames(df.tsne) <- c("X", "Y", "Z")}
    
  } else {
    df.tsne <- tsne_cache
    k <- ncol(df.tsne)
  }
  
  # Merge in covariate information
  if (!is.null(shape)) {
    df.tsne.m <- 
      merge(df.tsne, sam_table[, c(color, shape), 
                               drop=FALSE], by=0, all=TRUE)
    
    # When shape is required
    # Bypass duplicate colnames if color == shape
    shape <- colnames(df.tsne.m)[ncol(df.tsne.m)] 
    df.tsne.m[[shape]] <- as.factor(df.tsne.m[[shape]])
    
  } else {
    df.tsne.m <- 
      merge(df.tsne, sam_table[, color, drop=FALSE], by=0, all=TRUE)
    shape <- 'shape' # Referenced by plotly later
    df.tsne.m[[shape]] <- 1 # Constant results in omitting shape
  }
  
  # Plotly | Scatterplot
  if (k == 2) {
    
    # 2D Plot
    p <- plot_ly(df.tsne.m,
                 x = as.formula("~X"),
                 y = as.formula("~Y"),
                 mode = "markers",
                 color = as.formula(paste("~", color, sep = "")),
                 symbol = as.formula(paste("~", shape, sep = "")),
                 type = "scatter",
                 text = df.tsne.m$Row.names,
                 marker = list(size = 10))
  } else {
    
    # 3D Plot
    p <- plot_ly(df.tsne.m,
                 x = as.formula("~X"),
                 y = as.formula("~Y"),
                 z = as.formula("~Z"),
                 mode = "markers",
                 color = as.formula(paste("~", color, sep = "")),
                 symbol = as.formula(paste("~", shape, sep = "")),
                 symbols = c("circle", "square", "diamond", 
                             "cross", "square-open", "circle-open", 
                             "diamond-open", "x"),
                 type = "scatter3d",
                 text = df.tsne.m$Row.names,
                 marker = list(size = 6))
  }
  p$p <- NULL # To suppress a shiny warning
  return(list(plot=p, data=df.tsne))
}

