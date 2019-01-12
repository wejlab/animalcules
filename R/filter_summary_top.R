#' Data visualization by pie chart
#'
#' @param MAE A multi-assay experiment object
#' @param samples_discard The list of samples to filter
#' @param filter_type Either 'By Microbes' or 'By Metadata'
#' @param sample_condition Which condition to check e.g. 'AGE
#' @return A plotly object
#'
#' @examples
#' toy_data <- readRDS("data/MAE.rds")
#' result <- filter_summary_top(toy_data,
#'                              samples_discard = c("subject_2", "subject_4"),
#'                              filter_type = "By Microbes",
#'                              sample_condition = "AGE")
#' result
#'
#' @import dplyr
#' @import plotly
#' @import magrittr
#' @import reshape2
#' @import MultiAssayExperiment
#'
#' @export
filter_summary_top <- function(MAE,
                               samples_discard,
                               filter_type,
                               sample_condition) {

    # Subset the data
    MAE_subset <- mae_pick_samples(MAE = MAE, discard_samples = samples_discard)

    # Extract data
    microbe <- MAE_subset[['MicrobeGenetics']]
    host <- MAE_subset[['HostGenetics']]
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

    # Add count summary data to sample table
    sam_table[,"Reads"] = colSums(counts_table[,rownames(sam_table)])
    sam_table[,"Taxnum"] = apply(counts_table , 2, function(x) sum(x >= 1))

    # select filter type
    if (filter_type == "By Microbes") {
        cov <- "Taxnum"
    } else {
        cov <- sample_condition
    }

    # Use density plot if the variable has more than 8 unique values
    # Use pie chart if the variable has less than 8 unique values
    if (length(unique(unlist(sam_table[,cov]))) > 8) {
        vec <- unlist(sam_table[,cov])
        num.scatter <- plotly::plot_ly(y = vec,
                                       jitter = 0.3,
                                       pointpos = -1.8,
                                       boxpoints = 'all',
                                       marker = list(color = 'rgb(7,40,89)'),
                                       line = list(color = 'rgb(7,40,89)'),
                                       name = cov, type="box") %>%
                                       layout(title=cov)
        num.scatter$elementId <- NULL
        return(num.scatter)
    } else {
        cat.df = data.frame(table(sam_table[,cov]))
        cat.pie <- plotly::plot_ly(cat.df, labels = ~Var1, values = ~Freq,
                           type = "pie",
                           showlegend = FALSE) %>%
                           layout(title=cov)
        cat.pie$elementId <- NULL
        return(cat.pie)
    }
}









