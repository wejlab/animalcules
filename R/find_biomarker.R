#' Identify biomarkers
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param input_select_target_biomarker Which condition is the target condition
#' @param nfolds number of splits in CV
#' @param nrepeats number of CVs with different random splits
#' @param seed for repeatable research
#' @param percent_top_biomarker Top importance percentage to pick biomarker
#' @param model_name one of "svm", "logistic regression", "gbm", "random forest"
#'
#' @return A list
#'
#' @examples
#' toy_data <- readRDS("data/MAE.rds")
#' p <- find_biomarker(toy_data,
#'                     tax_level="genus",
#'                     input_select_target_biomarker=c("DISEASE"),
#'                     nfolds = 3,
#'                     nrepeats = 3,
#'                     seed = 99,
#'                     percent_top_biomarker = 0.2,
#'                     model_name = "logistic regression")
#' p
#'
#' @import caret
#' @import tibble
#' @import gbm
#' @import caret
#' @import ggplot2
#' @import dplyr
#' @import MultiAssayExperiment
#'
#' @export

find_biomarker <- function(MAE,
                           tax_level,
                           input_select_target_biomarker,
                           nfolds = 3,
                           nrepeats = 3,
                           seed = 99,
                           percent_top_biomarker = 0.2,
                           model_name = c("svm", "logistic regression", "gbm", "random forest")) {

    ## SEED
    set.seed(seed)

    ## tables from MAE
    microbe <- MAE[['MicrobeGenetics']] #double bracket subsetting is easier
    tax_table <- as.data.frame(rowData(microbe)) # organism x taxlev
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    counts_table <- as.data.frame(assays(microbe))[,rownames(sam_table)] # organism x sample

    ## shiny UI input object


    # Sum counts by taxon level and return log10 cpm
    logcpm_table <- counts_table %>%
                        upsample_counts(tax_table, tax_level) %>%
                        counts_to_logcpm() %>%
                        base::t() %>%
                        base::as.data.frame()


    # add target variable
    logcpm_table[,'y'] <- sam_table %>%
                            dplyr::pull(input_select_target_biomarker)

    # set up classification model prameters
    fitControl <- caret::trainControl(## n1-fold CV
                               method = "repeatedcv",
                               number = nfolds,
                               ## repeated n2 times
                               repeats = nrepeats,
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary,
                               sampling = "smote",
                               savePredictions = TRUE)

    # choose different model
    if (model_name == "logistic regression"){
        model_fit <- caret::train(y ~ .,
                    data = logcpm_table,
                    method = "glmnet",
                    tuneLength = 5,
                    trControl = fitControl,
                    metric = "ROC")
    } else if (model_name == "svm"){
        model_fit <- caret::train(y ~ .,
                    data = logcpm_table,
                    method = "svmLinear",
                    tuneLength = 5,
                    trControl = fitControl,
                    metric = "ROC")
    } else if (model_name == "gbm"){
        model_fit <- caret::train(y ~ .,
                     data = logcpm_table,
                     method = "gbm",
                     trControl = fitControl,
                     tuneLength = 5,
                     metric = "ROC",
                     ## This last option is actually one
                     ## for gbm() that passes through
                     verbose = FALSE)
    } else if (model_name == "random forest"){
        model_fit <- caret::train(y ~ .,
                    data = logcpm_table,
                    method = "ranger",
                    trControl = fitControl,
                    tuneLength = 5,
                    metric = "ROC",
                    # ranger specific parameter
                    importance = "impurity")
    }

    # process the importance score
    if (model_name == "svm"){
        svm_importance <- caret::varImp(model_fit)$importance
        svm_importance[,2] <- NULL
        colnames(svm_importance) <- "importance"

        biomarker <- svm_importance %>%
                            rownames_to_column() %>%
                            rename(biomarker = rowname) %>%
                            arrange(importance) %>%
                            filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                            select(biomarker) %>%
                            .$biomarker


        importance_plot <- svm_importance %>%
                            rownames_to_column() %>%
                            rename(biomarker = rowname) %>%
                            arrange(importance) %>%
                            filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                            mutate(biomarker = forcats::fct_inorder(biomarker)) %>%
                            ggplot2::ggplot()+
                            geom_col(aes(x = biomarker, y = importance))+
                            coord_flip()+
                            theme_bw()

    } else{

        biomarker <- caret::varImp(model_fit)$importance %>%
                          base::as.data.frame() %>%
                          rownames_to_column() %>%
                          rename(importance = Overall) %>%
                          rename(biomarker = rowname) %>%
                          arrange(importance) %>%
                          filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                          select(biomarker) %>%
                          .$biomarker

        importance_plot <- caret::varImp(model_fit)$importance %>%
                          base::as.data.frame() %>%
                          rownames_to_column() %>%
                          rename(importance = Overall) %>%
                          rename(biomarker = rowname) %>%
                          arrange(importance) %>%
                          filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                          mutate(biomarker = forcats::fct_inorder(biomarker)) %>%
                          ggplot2::ggplot()+
                            geom_col(aes(x = biomarker, y = importance))+
                            coord_flip()+
                            theme_bw()
    }

    # retrain the model using the biomarker
    logcpm_table <- logcpm_table %>%
                        select(biomarker,y)

    # choose different model
    if (model_name == "logistic regression"){
        model_fit <- caret::train(y ~ .,
                    data = logcpm_table,
                    method = "glmnet",
                    tuneLength = 5,
                    trControl = fitControl,
                    metric = "ROC")
    } else if (model_name == "svm"){
        model_fit <- caret::train(y ~ .,
                    data = logcpm_table,
                    method = "svmLinear",
                    tuneLength = 5,
                    trControl = fitControl,
                    metric = "ROC")
    } else if (model_name == "gbm"){
        model_fit <- caret::train(y ~ .,
                     data = logcpm_table,
                     method = "gbm",
                     trControl = fitControl,
                     tuneLength = 5,
                     metric = "ROC",
                     ## This last option is actually one
                     ## for gbm() that passes through
                     verbose = FALSE)
    } else if (model_name == "random forest"){
        model_fit <- caret::train(y ~ .,
                    data = logcpm_table,
                    method = "ranger",
                    trControl = fitControl,
                    tuneLength = 5,
                    metric = "ROC",
                    # ranger specific parameter
                    importance = "impurity")
    }



    # print the biomarker CV performance
    biomarker_cv_performance <- model_fit$results %>%
        select(ROC, Sens, Spec) %>%
        filter(ROC == max(ROC))


    # output a list
    list_output <- list(biomarker = biomarker,
                        importance_plot = importance_plot,
                        biomarker_cv_performance = biomarker_cv_performance[1,])
    return(list_output)

}
