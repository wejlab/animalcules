data_dir = system.file("extdata/MAE.rds", package = "animalcules")
MAE = readRDS(data_dir)

alpha.methods <-  c("inverse_simpson", "gini_simpson", "shannon", "unit")
beta.methods <- c("wUniFrac", "bray")

tax.default <- "genus"
# These are options for rendering datatables
dtopts <- list(scrollX=TRUE, paging=TRUE)

tax.name <- colnames(rowData(MAE[['MicrobeGenetics']]))
sam.name <- rownames(colData(MAE[['MicrobeGenetics']]))
org.name <- rownames(as.data.frame(assays(MAE[['MicrobeGenetics']])))

measure.type <- c('Final Guess', 'Final Best Hit', 'Final High Confidence Hit')
minbatch <- function(batch1){
    batch2 <- as.factor(batch1)
    batch3 <- split(batch1,batch2)
    return(min(unlist(lapply(seq_len(length(batch3)),
        function(x) length(batch3[[x]])))))
}

covariates = colnames(colData(MAE))

sam_temp <- as.data.frame(colData(MAE[['MicrobeGenetics']]))
num_select <- lapply(covariates, function(x) is_categorical(unlist(sam_temp[,x])))
num_covariates <- covariates[!unlist(num_select)]

# choose the covariates that has less than 8 levels
covariates.colorbar <- c()
for (i in seq_len(length(covariates))){
  num.levels <- length(unique(colData(MAE)[[covariates[i]]]))
  if (num.levels < 8){
    covariates.colorbar <- c(covariates.colorbar, covariates[i])
  }
}

# choose the covariates that has 2 levels
covariates.two.levels <- c()
for (i in seq_len(length(covariates))){
  num.levels <- length(unique(colData(MAE)[[covariates[i]]]))
  if (num.levels == 2){
    covariates.two.levels <- c(covariates.two.levels, covariates[i])
  }
}

# assays
mae.assays <- names(MAE)

# enrichR dbList
dbList <- c(#"GO_Biological_Process_2018", 
            "WikiPathways_2019_Human", 
            "KEGG_2019_Human", 
            "Panther_2016")

# msigdb gene set collections and sub-collections
collections <- list(
  "H" = "Hallmark",
  "C1" = "Positional",
  "C2" = "Curated",
  "C3" = "Regulatory Targets",
  "C4" = "Computational",
  "C5" = "Ontology",
  "C6" = "Oncogenic",
  "C7" = "Immunologic",
  "C8" = "Cell Type"
)

sub_collections <- list(
  "C2" = c(
    "CGP",
    "CP:BIOCARTA",
    "CP:KEGG",
    "CP:PID",
    "CP:REACTOME",
    "CP:WIKIPATHWAYS"
  ),
  "C3"= c(
    "MIR:MIR_Legacy",
    "MIR:MIRDB",
    "TFT:GTRD",
    "TFT:TFT_Legacy"
  ),
  "C4" = c(
    "CGN",
    "CM"
  ),
  "C5" = c(
    "GO:BP",
    "GO:CC",
    "GO:MF",
    "HPO"
  )
)

