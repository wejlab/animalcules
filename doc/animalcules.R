## --------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)

## --------------------------------------------------------------------------
library(magrittr)
library(tibble)
library(MultiAssayExperiment)
library(SummarizedExperiment)
library(S4Vectors)
library(usethis)
library(dplyr)
library(plotly)
library(tsne)
library(reshape2)
library(caret)
library(vegan)
library(ape)
library(DESeq2)
library(ggplot2)
library(forcats)
library(grid)
library(lattice)
library(plotROC)
library(animalcules)


## --------------------------------------------------------------------------
data_dir = system.file("extdata/MAE.rds", package = "animalcules")
MAE = readRDS(data_dir)

## --------------------------------------------------------------------------
# run_animalcules()

## --------------------------------------------------------------------------
p <- filter_summary_top(MAE,
                        samples_discard = c("subject_2", "subject_4"),
                        filter_type = "By Metadata",
                        sample_condition = "AGE")
p

## --------------------------------------------------------------------------
p <- filter_summary_bottom(MAE,
                           samples_discard = c("subject_2", "subject_4"),
                           filter_type = "By Metadata",
                           sample_condition = "SEX")
p

## --------------------------------------------------------------------------
microbe <- MAE[['MicrobeGenetics']]
samples <- as.data.frame(colData(microbe))
result <- filter_categorize(samples,
                            sample_condition = "AGE",
                            new_label="AGE_GROUP",
                            bin_breaks=c(0,55,75,100),
                            bin_labels=c('Young','Adult',"Elderly"))
result$sam_table
result$plot.unbinned
result$plot.binned

## --------------------------------------------------------------------------
p <- relabu_barplot(MAE,
                    tax_level="family",
                    order_organisms=c('Retroviridae'),
                    sort_by="organisms",
                    sample_conditions=c('SEX', 'AGE'),
                    show_legend=TRUE)
p

## --------------------------------------------------------------------------
p <- relabu_heatmap(MAE,
                   tax_level="genus",
                   sort_by="conditions",
                   sample_conditions=c("SEX", "AGE"))
p

## --------------------------------------------------------------------------
p <- relabu_boxplot(MAE,
                    tax_level="genus",
                    organisms=c("Escherichia", "Actinomyces"),
                    condition="SEX",
                    datatype="logcpm")
p

## --------------------------------------------------------------------------
alpha_div_boxplot(MAE = MAE,
                  tax_level = "genus",
                  condition = "DISEASE",
                  alpha_metric = "shannon")

## --------------------------------------------------------------------------
do_alpha_div_test(MAE = MAE,
                  tax_level = "genus",
                  condition = "DISEASE",
                  alpha_metric = "shannon",
                  alpha_stat = "T-test")

## --------------------------------------------------------------------------
diversity_beta_heatmap(MAE = MAE, 
                       tax_level = 'genus', 
                       input_beta_method = "bray",
                       input_bdhm_select_conditions = 'DISEASE',
                       input_bdhm_sort_by = 'condition')

## --------------------------------------------------------------------------
diversity_beta_boxplot(MAE = MAE, 
                       tax_level = 'genus', 
                       input_beta_method = "bray",
                       input_select_beta_condition = 'DISEASE')

## --------------------------------------------------------------------------
diversity_beta_test(MAE = MAE, 
                    tax_level = 'genus',
                    input_beta_method = "bray",
                    input_select_beta_condition =  'DISEASE',
                    input_select_beta_stat_method = 'PERMANOVA',
                    input_num_permutation_permanova = 999)

## --------------------------------------------------------------------------
result <- dimred_pca(MAE,
                     tax_level="genus",
                     color="AGE",
                     shape="DISEASE",
                     pcx=1,
                     pcy=2,
                     datatype="logcpm")
#result$plot
result$table

## --------------------------------------------------------------------------
result <- dimred_pcoa(MAE,
                      tax_level="genus",
                      color="AGE",
                      shape="DISEASE",
                      axx=1,
                      axy=2,
                      method="bray")
#result$plot
result$table

## --------------------------------------------------------------------------
p <- dimred_tsne(MAE,
                 tax_level="phylum",
                 color="AGE",
                 shape="GROUP",
                 k="3D",
                 initial_dims=30,
                 perplexity=10,
                 datatype="logcpm")
p

## --------------------------------------------------------------------------
p <- differential_abundance(MAE,
                            tax_level="phylum",
                            input_da_condition=c("DISEASE"),
                            min_num_filter = 2,
                            input_da_padj_cutoff = 0.5)
p

## --------------------------------------------------------------------------
p <- find_biomarker(MAE,
                    tax_level="genus",
                    input_select_target_biomarker=c("SEX"),
                    nfolds = 3,
                    nrepeats = 3,
                    seed = 99,
                    percent_top_biomarker = 0.2,
                    model_name = "logistic regression")
# biomarker
p$biomarker

# importance plot
p$importance_plot

# ROC plot
p$roc_plot

## --------------------------------------------------------------------------
# data_raw <-
#     base::system.file("extdata/animalcules.rds", package = "animalcules") %>%
#     base::readRDS()
# 
# se_mgx <-
#     magrittr::use_series(data_raw, count_table) %>%
#     base::data.matrix() %>%
#     S4Vectors::SimpleList() %>%
#     magrittr::set_names("MGX")
# 
# se_ge <-
#     magrittr::use_series(data_raw, gene_expression_table) %>%
#     base::data.matrix() %>%
#     S4Vectors::SimpleList() %>%
#     magrittr::set_names("GeneExpression")
# 
# se_colData <-
#     magrittr::use_series(data_raw, metadata_table) %>%
#     S4Vectors::DataFrame()
# 
# se_rowData <-
#     magrittr::use_series(data_raw, tax_table) %>%
#     base::data.frame() %>%
#     dplyr::mutate_all(as.character) %>%
#     dplyr::select(superkingdom, phylum, class, order, family, genus) %>%
#     S4Vectors::DataFrame()
# 
# microbe_se <-
#     SummarizedExperiment::SummarizedExperiment(assays = se_mgx,
#                                                colData = se_colData,
#                                                rowData = se_rowData)
# 
# host_se <-
#     SummarizedExperiment::SummarizedExperiment(assays = se_ge,
#                                                colData = se_colData)
# 
# mae_experiments <-
#     S4Vectors::SimpleList(MicrobeGenetics = microbe_se, HostGenetics = host_se)
# 
# MAE <-
#     MultiAssayExperiment::MultiAssayExperiment(experiments = mae_experiments,
#                                                colData = se_colData)

## usethis::use_data(MAE)

# saveRDS(MAE, "extdata/MAE.rds")

## --------------------------------------------------------------------------
sessionInfo()

