

# reactive values shared thorough the shiny app
vals <- reactiveValues(
    data_dir = system.file("data/MAE.rds", package = "animalcules"),
    MAE = readRDS(data_dir),
    MAE_backup = MAE
)

# update taxonomy levels
updateTaxLevel <- function(){
  MAE <- vals$MAE
  updateSelectInput(session, "taxl",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl.alpha",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl.beta",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl.pca",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl.da",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl.edger",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl.pa",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "sra_taxlev",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "hmra_taxlev",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl_single_species",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxlTable",
                    choices = rownames(MAE[['MicrobeGenetics']]))
  updateSelectInput(session, "taxl_biomarker",
                    choices = rownames(MAE[['MicrobeGenetics']]))
}

# update samples
updateSample <- function(){
    MAE <- vals$MAE
    updateSelectInput(session, "filterSample",
                      choices = colnames(MAE[['MicrobeGenetics']]))
    updateSelectInput(session, "hmra_isolate_samples",
                      choices = colnames(MAE[['MicrobeGenetics']]))
    updateSelectInput(session, "sra_isolate_samples",
                      choices = colnames(MAE[['MicrobeGenetics']]))
}



# update covariate names
updateCovariate <- function(){
    MAE <- vals$MAE
    covariates <- colnames(colData(MAE))
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

    ## numeric cov
    sam_temp <- colData(MAE)
    num_select <- lapply(covariates, function(x) is.categorical(unlist(sam_temp[,x])))
    num_covariates <- covariates[!unlist(num_select)]

    updateSelectInput(session, "select_covariate_condition_biomarker",
                      choices = covariates)
    updateSelectInput(session, "select_single_species_condition",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_target_condition_biomarker",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_condition_sample_filter",
                      choices = c("Reads", covariates))
    updateSelectInput(session, "select_condition_sample_filter_micro",
                      choices = c("Taxon elements number", covariates))
    updateSelectInput(session, "select_condition_sample_filter_sidebar",
                      choices = c("Reads", covariates))
    updateSelectInput(session, "select_condition_sample_distribution",
                      choices = covariates)
    updateSelectInput(session, "select_condition",
                      choices = covariates)
    updateSelectInput(session, "select_heatmap_condition_1",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_heatmap_condition_2",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_alpha_div_condition",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_beta_condition",
                      choices = covariates.two.levels)
    updateSelectInput(session, "select_beta_heatmap_condition_1",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_beta_heatmap_condition_2",
                      choices = covariates.colorbar)
    updateSelectInput(session, "select_pca_color",
                      choices = covariates)
    updateSelectInput(session, "select_pca_shape",
                      choices = c("None", covariates.colorbar))
    updateSelectInput(session, "da.condition",
                      choices = covariates)
    updateSelectInput(session, "edger.condition",
                      choices = covariates.colorbar)
    updateSelectInput(session, "da.condition.covariate",
                      choices = covariates)
    updateSelectInput(session, "pa.condition",
                      choices = covariates.colorbar)
    updateSelectInput(session, "sra_select_conditions",
                      choices = covariates)
    updateSelectInput(session, "gra_select_conditions",
                      choices = c("All", covariates))
    updateSelectInput(session, "hmra_select_conditions",
                      choices = covariates)
    updateSelectInput(session, "bin_cov",
                      choices = num_covariates)
    updateSelectInput(session, "bdhm_select_conditions",
                      choices = covariates.colorbar)

}
