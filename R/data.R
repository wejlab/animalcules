#' Toy Host/Microbiome MultiAssayExperiment (MAE)
#'
#' A toy multi-assay experiment object with mock data
#' that is useful for tesing animacules functionality
#'
#' @format MAE object with 4 data attributes
#' \describe{
#'   \item{ExperimentList}{A list of experiments represented as SummarizedExperiment objects}
#'   \item{colData}{A dataframe of samples and phenotypic attributes}
#'   \item{sampleMap}{A dataframe of samples mapped to experiments}
#'   \item{metadata}{Metadata information regarding experiments}
#' }
"MAE"