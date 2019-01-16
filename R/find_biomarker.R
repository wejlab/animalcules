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
#' @import caret
#' @importFrom ggplot2 geom_col aes coord_flip theme_bw
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
    # bioC not suggesst add set seed function in R code
    # set.seed(seed)

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

    print(1)
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
    print(2)
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
    print(3)
    # process the importance score
    if (model_name == "svm"){
        svm_importance <- caret::varImp(model_fit)$importance
        svm_importance[,2] <- NULL
        colnames(svm_importance) <- "importance"

        biomarker <- svm_importance %>%
                            tibble::rownames_to_column() %>%
                            dplyr::rename(biomarker = rowname) %>%
                            dplyr::arrange(importance) %>%
                            dplyr::filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                            dplyr::select(biomarker) %>%
                            .$biomarker


        importance_plot <- svm_importance %>%
                            tibble::rownames_to_column() %>%
                            dplyr::rename(biomarker = rowname) %>%
                            dplyr::arrange(importance) %>%
                            dplyr::filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                            dplyr::mutate(biomarker = forcats::fct_inorder(biomarker)) %>%
                            ggplot2::ggplot()+
                            geom_col(aes(x = biomarker, y = importance))+
                            coord_flip()+
                            theme_bw()

    } else{

        biomarker <- caret::varImp(model_fit)$importance %>%
                          base::as.data.frame() %>%
                          tibble::rownames_to_column() %>%
                          dplyr::rename(importance = Overall) %>%
                          dplyr::rename(biomarker = rowname) %>%
                          dplyr::arrange(importance) %>%
                          dplyr::filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                          dplyr::select(biomarker) %>%
                          .$biomarker

        importance_plot <- caret::varImp(model_fit)$importance %>%
                          base::as.data.frame() %>%
                          tibble::rownames_to_column() %>%
                          dplyr::rename(importance = Overall) %>%
                          dplyr::rename(biomarker = rowname) %>%
                          dplyr::arrange(importance) %>%
                          dplyr::filter(importance > quantile(importance, 1-percent_top_biomarker)) %>%
                          dplyr::mutate(biomarker = forcats::fct_inorder(biomarker)) %>%
                          ggplot2::ggplot()+
                            geom_col(aes(x = biomarker, y = importance))+
                            coord_flip()+
                            theme_bw()
    }
    print(4)
    # retrain the model using the biomarker
    logcpm_table <- logcpm_table %>%
                        dplyr::select(biomarker,y)

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

    print(5)

    # print the biomarker CV performance
    biomarker_cv_performance <- model_fit$results %>%
        dplyr::select(ROC, Sens, Spec) %>%
        dplyr::filter(ROC == max(ROC))
    print(6)

    # output a list
    list_output <- list(biomarker = biomarker,
                        importance_plot = importance_plot,
                        biomarker_cv_performance = biomarker_cv_performance[1,])
    print(7)
    return(list_output)

}
