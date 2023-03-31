## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(comment = "#", message = FALSE)
devtools::load_all(".")
library(SummarizedExperiment)

## ----get_package, eval=FALSE--------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE)) {
#      install.packages("BiocManager")
#  }
#  BiocManager::install("compbiomed/animalcules")

## ---- eval=FALSE--------------------------------------------------------------
#  if (!requireNamespace("devtools", quietly = TRUE)) {
#      install.packages("devtools")
#  }
#  devtools::install_github("compbiomed/animalcules")

## ----load, eval=FALSE---------------------------------------------------------
#  library(animalcules)
#  library(SummarizedExperiment)

## ---- eval=FALSE--------------------------------------------------------------
#  run_animalcules()

## -----------------------------------------------------------------------------
data_dir <- system.file("extdata/TB_example_dataset.rds", 
    package = "animalcules")
MAE <- readRDS(data_dir)

## ---- eval=FALSE--------------------------------------------------------------
#  data_dir <- "PATH_TO_THE_ANIMALCULES_FILE"
#  MAE <- readRDS(data_dir)

## -----------------------------------------------------------------------------
p <- filter_summary_pie_box(MAE,
    samples_discard = c("SRR1204622"),
    filter_type = "By Metadata",
    sample_condition = "age_s"
)
p

## -----------------------------------------------------------------------------
p <- filter_summary_bar_density(MAE,
    samples_discard = c("SRR1204622"),
    filter_type = "By Metadata",
    sample_condition = "sex_s"
)
p

## -----------------------------------------------------------------------------
microbe <- MAE[["MicrobeGenetics"]]
samples <- as.data.frame(colData(microbe))
result <- filter_categorize(samples,
    sample_condition = "age_s",
    new_label = "AGE_GROUP",
    bin_breaks = c(0, 30, 40, 100),
    bin_labels = c("a", "b", "c")
)
head(result$sam_table)
result$plot.unbinned
result$plot.binned

## -----------------------------------------------------------------------------
p <- relabu_barplot(MAE,
    tax_level = "genus",
    sort_by = "conditions",
    sample_conditions = c("Disease"),
    show_legend = TRUE
)
p

## -----------------------------------------------------------------------------
p <- relabu_heatmap(MAE,
    tax_level = "genus",
    sort_by = "conditions",
    sample_conditions = c("sex_s", "age_s")
)
p

## -----------------------------------------------------------------------------
p <- relabu_boxplot(MAE,
    tax_level = "genus",
    organisms = c("Streptococcus", "Staphylococcus"),
    condition = "sex_s",
    datatype = "logcpm"
)
p

## -----------------------------------------------------------------------------
alpha_div_boxplot(
    MAE = MAE,
    tax_level = "genus",
    condition = "Disease",
    alpha_metric = "shannon"
)

## -----------------------------------------------------------------------------
do_alpha_div_test(
    MAE = MAE,
    tax_level = "genus",
    condition = "Disease",
    alpha_metric = "shannon",
    alpha_stat = "T-test"
)

## -----------------------------------------------------------------------------
diversity_beta_heatmap(
    MAE = MAE,
    tax_level = "genus",
    input_beta_method = "bray",
    input_bdhm_select_conditions = "Disease",
    input_bdhm_sort_by = "condition"
)

## -----------------------------------------------------------------------------
diversity_beta_boxplot(
    MAE = MAE,
    tax_level = "genus",
    input_beta_method = "bray",
    input_select_beta_condition = "Disease"
)

## -----------------------------------------------------------------------------
diversity_beta_test(
    MAE = MAE,
    tax_level = "genus",
    input_beta_method = "bray",
    input_select_beta_condition = "Disease",
    input_select_beta_stat_method = "PERMANOVA",
    input_num_permutation_permanova = 999
)

## -----------------------------------------------------------------------------
result <- dimred_pca(MAE,
    tax_level = "genus",
    color = "age_s",
    shape = "Disease",
    pcx = 1,
    pcy = 2,
    datatype = "logcpm"
)
result$plot
head(result$table)

## -----------------------------------------------------------------------------
result <- dimred_pcoa(MAE,
    tax_level = "genus",
    color = "age_s",
    shape = "Disease",
    axx = 1,
    axy = 2,
    method = "bray"
)
result$plot
head(result$table)

## -----------------------------------------------------------------------------
result <- dimred_umap(MAE,
    tax_level = "genus",
    color = "age_s",
    shape = "Disease",
    cx = 1,
    cy = 2,
    n_neighbors = 15,
    metric = "euclidean",
    datatype = "logcpm"
)
result$plot

## -----------------------------------------------------------------------------
# result <- dimred_tsne(MAE,
#                       tax_level="phylum",
#                       color="age_s",
#                       shape="Disease",
#                       k="3D",
#                       initial_dims=30,
#                       perplexity=10,
#                       datatype="logcpm")
# result$plot

## -----------------------------------------------------------------------------
p <- differential_abundance(MAE,
    tax_level = "phylum",
    input_da_condition = c("Disease"),
    min_num_filter = 2,
    input_da_padj_cutoff = 0.5
)
p

## -----------------------------------------------------------------------------
p <- find_biomarker(MAE,
    tax_level = "genus",
    input_select_target_biomarker = c("Disease"),
    nfolds = 3,
    nrepeats = 3,
    seed = 99,
    percent_top_biomarker = 0.2,
    model_name = "logistic regression"
)
# biomarker
p$biomarker

# importance plot
p$importance_plot

# ROC plot
p$roc_plot

## -----------------------------------------------------------------------------
sessionInfo()

