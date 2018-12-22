library(shiny)
library(shinyjs)
library(plotly)
# source(file.path("utils", "helpers.R"))



alpha.methods <- c("Shannon", "Simpson", "InvSimpson")
# Weigthed Unifrac, Bray-Curtis
beta.methods <- c("wUniFrac", "bray")

tax.name <- c('superkingdom', 'kingdom', 'phylum', 'class', 'order', 'family',
    'genus', 'species', 'no rank')
norm.methods <- c('EBayes coreOTU Normalization',
    'Quantile coreOTU Normalization', 'Library Size Scaling')
measure.type <- c('Final Guess', 'Final Best Hit', 'Final High Confidence Hit')
minbatch <- function(batch1){
    batch2 <- as.factor(batch1)
    batch3 <- split(batch1,batch2)
    return(min(unlist(lapply(1:length(batch3),
        function(x) length(batch3[[x]])))))
}


#
# # choose the covariates that has less than 8 levels
# covariates.colorbar <- c()
# for (i in 1:length(covariates)){
#   num.levels <- length(unique(sample_data(pstat)[[covariates[i]]]))
#   if (num.levels < 8){
#     covariates.colorbar <- c(covariates.colorbar, covariates[i])
#   }
# }
#
# # choose the covariates that has 2 levels
# covariates.two.levels <- c()
# for (i in 1:length(covariates)){
#   num.levels <- length(unique(sample_data(pstat)[[covariates[i]]]))
#   if (num.levels == 2){
#     covariates.two.levels <- c(covariates.two.levels, covariates[i])
#   }
# }
#
# is.categorical <- function(v) {
#   if (class(v) == "integer" || class(v) == "numeric") {
#     return(F)
#   } else {
#     return(T)
#   }
# }
#
# # numeric cov
#     sam_temp <- as.data.frame(pstat@sam_data)
#     num_select <- lapply(covariates, function(x) is.categorical(unlist(sam_temp[,x])))
#     num_covariates <- covariates[!unlist(num_select)]
#
#
#
# maxbatchElems <- minbatch(c(pstat@sam_data[,1])[[1]])
# maxcondElems <- minbatch(c(pstat@sam_data[,2])[[1]])
# defaultDisp <- 30
# defaultGenesDisp <- 10
# maxGenes <- dim(pstat@otu_table)[1]
#
#
#
#
# #sample name
# sample.names.all <- colnames(pstat@otu_table@.Data)
