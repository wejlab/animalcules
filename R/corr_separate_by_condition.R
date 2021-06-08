#' Splits a MultiAssayExperiment object according to its colData
#' 
#' @param MAE A MultiAssayExperiment object 
#' @param condition One of the covariates listed in the colData of the MultiAssayExperiment. Must have two different levels. 
#' @return Two MultiAssayExperiment objects, split apart from the original
#' 
#' @examples
#' data_dir = system.file('extdata/MAE.rds', package = 'animalcules')
#' toy_data <- readRDS(data_dir)
#' colData(toy_data) # "DISEASE" is a condition with 2 levels
#' toy_data_split <- separate_by_condition(toy_data, "DISEASE")
#' toy_data_split[1]
#' toy_data_split[2]
#' 
#' @import MultiAssayExperiment
#' 
#' @export

separate_by_condition <- function(MAE, condition) {
  status1 <- unique(colData(MAE)[condition])[1,]
  status2 <- unique(colData(MAE)[condition])[2,]
  con1 <- subsetByColData(MAE, colData(MAE)[[paste(condition)]]==status1)
  con2 <- subsetByColData(MAE, colData(MAE)[[paste(condition)]]==status2)
  return(list(con1, con2))
}