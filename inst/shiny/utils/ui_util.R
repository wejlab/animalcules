data_dir = system.file("extdata/MAE.rds", package = "animalcules")
MAE = readRDS(data_dir)


alpha.methods <-  c("inverse_simpson", "gini_simpson", "shannon")
beta.methods <- c("wUniFrac", "bray")

tax.default <- "genus"

tax.name <- colnames(rowData(MAE[['MicrobeGenetics']]))
sam.name <- rownames(colData(MAE[['MicrobeGenetics']]))
org.name <- rownames(as.data.frame(assays(MAE[['MicrobeGenetics']])))

measure.type <- c('Final Guess', 'Final Best Hit', 'Final High Confidence Hit')
minbatch <- function(batch1){
    batch2 <- as.factor(batch1)
    batch3 <- split(batch1,batch2)
    return(min(unlist(lapply(1:length(batch3),
        function(x) length(batch3[[x]])))))
}


covariates = colnames(colData(MAE))

sam_temp <- as.data.frame(colData(MAE[['MicrobeGenetics']]))
num_select <- lapply(covariates, function(x) is.categorical(unlist(sam_temp[,x])))
num_covariates <- covariates[!unlist(num_select)]


# choose the covariates that has less than 8 levels
covariates.colorbar <- c()
for (i in 1:length(covariates)){
  num.levels <- length(unique(colData(MAE)[[covariates[i]]]))
  if (num.levels < 8){
    covariates.colorbar <- c(covariates.colorbar, covariates[i])
  }
}

# choose the covariates that has 2 levels
covariates.two.levels <- c()
for (i in 1:length(covariates)){
  num.levels <- length(unique(colData(MAE)[[covariates[i]]]))
  if (num.levels == 2){
    covariates.two.levels <- c(covariates.two.levels, covariates[i])
  }
}


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
