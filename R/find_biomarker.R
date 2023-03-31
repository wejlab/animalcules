#' Identify biomarkers
#'
#' @param MAE A multi-assay experiment object
#' @param tax_level The taxon level used for organisms
#' @param input_select_target_biomarker Which condition is the target condition
#' @param nfolds number of splits in CV
#' @param nrepeats number of CVs with different random splits
#' @param seed for repeatable research
#' @param percent_top_biomarker Top importance percentage to pick biomarker
#' @param model_name one of 'logistic regression', 'random forest'
#'
#' @return A list
#'
#' @examples
#' data_dir <- system.file("extdata/MAE.rds", package = "animalcules")
#' toy_data <- readRDS(data_dir)
#' p <- find_biomarker(toy_data,
#'   tax_level = "family",
#'   input_select_target_biomarker = c("DISEASE"),
#'   nfolds = 3,
#'   nrepeats = 3,
#'   seed = 99,
#'   percent_top_biomarker = 0.2,
#'   model_name = "logistic regression"
#' )
#' p
#'
#' @export
find_biomarker <- function(MAE,
    tax_level,
    input_select_target_biomarker,
    nfolds = 3,
    nrepeats = 3,
    seed = 99,
    percent_top_biomarker = 0.2,
    model_name = c(
        "logistic regression",
        "random forest"
    )) {
    
    ## SEED bioC not suggesst add set seed function in R code set.seed(seed)
    ## tables from MAE
    microbe <- MAE[["MicrobeGenetics"]] # double bracket subsetting is easier
    tax_table <- SummarizedExperiment::rowData(microbe) |>
        as.data.frame()
    sam_table <- SummarizedExperiment::colData(microbe) |>
        as.data.frame()
    counts_table <- SummarizedExperiment::assays(microbe) |>
        as.data.frame() |> 
        dplyr::select(dplyr::all_of(rownames(sam_table)))
    ## shiny UI input object
    # Sum counts by taxon level and return log10 cpm
    logcpm_table <- counts_table |>
        upsample_counts(tax_table, tax_level) |>
        counts_to_logcpm() |> base::t() |> base::as.data.frame()
    # add target variable
    logcpm_table[, "y"] <-
        sam_table |> dplyr::pull(input_select_target_biomarker)
    # set up classification model prameters
    fitControl <- caret::trainControl(
        method = "repeatedcv",
        number = nfolds,
        repeats = nrepeats,
        classProbs = TRUE,
        summaryFunction = caret::twoClassSummary,
        savePredictions = TRUE
    )
    # choose different model
    if (model_name == "logistic regression") {
        model_fit <-
            caret::train(y ~ ., data = logcpm_table, method = "glmnet",
                tuneLength = 5, trControl = fitControl, metric = "ROC")
    } else if (model_name == "svm") {
        model_fit <- caret::train(y ~ ., data = logcpm_table,
            method = "svmLinear", tuneLength = 5, 
            trControl = fitControl, metric = "ROC")
    } else if (model_name == "gbm") {
        model_fit <- caret::train(y ~ ., data = logcpm_table, method = "gbm",
            trControl = fitControl, tuneLength = 5,
            metric = "ROC", verbose = FALSE)
    } else if (model_name == "random forest") {
        model_fit <- caret::train(y ~ ., data = logcpm_table, method = "ranger",
            trControl = fitControl, tuneLength = 5,
            metric = "ROC", importance = "impurity")
    }
    # process the importance score
    if (model_name == "svm") {
        svm_importance <- caret::varImp(model_fit)$importance
        svm_importance[, 2] <- NULL
        colnames(svm_importance) <- "importance"
        biom_pre <- svm_importance |> tibble::rownames_to_column() |>
            dplyr::rename("biomarker" = "rowname") |>
            dplyr::arrange("importance")
        ind <- biom_pre$importance > quantile(biom_pre$importance, 
            1 - percent_top_biomarker)
        biomarker <- biom_pre |>  dplyr::filter(ind) |>
            dplyr::pull(biomarker)
        imp_plot_pre <- biom_pre |> dplyr::filter(ind) |>
            dplyr::mutate(biomarker = forcats::fct_inorder(biomarker))
        importance_plot <- ggplot2::ggplot(imp_plot_pre) +
            ggplot2::geom_col(ggplot2::aes_string(x = 'biomarker', y = 'importance')) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw()
    } else {
        biom_pre <- caret::varImp(model_fit)$importance |>
            base::as.data.frame() |>
            tibble::rownames_to_column() |>
            dplyr::rename("importance" = "Overall") |>
            dplyr::rename("biomarker" = "rowname") |>
            dplyr::arrange("importance")
        ind <- biom_pre$importance >
            quantile(biom_pre$importance, 1 - percent_top_biomarker)
        biomarker <- biom_pre |> dplyr::filter(ind) |>
            dplyr::pull(biomarker) 
        
        importance_plot <-  biom_pre |> dplyr::filter(ind) |>
            dplyr::mutate(biomarker = forcats::fct_inorder(biomarker)) |>
            ggplot2::ggplot() +
            ggplot2::geom_col(ggplot2::aes_string(x = 'biomarker', y = 'importance')) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw()
    }
    # retrain the model using the biomarker
    logcpm_table <- logcpm_table |>
        dplyr::select(dplyr::all_of(biomarker), "y")
    # choose different model
    if (model_name == "logistic regression") {
        model_fit <- caret::train(
            y ~ ., data = logcpm_table, method = "glmnet",
            tuneLength = 5, trControl = fitControl, metric = "ROC")
    } else if (model_name == "svm") {
        model_fit <- caret::train(
            y ~ ., data = logcpm_table, method = "svmLinear",
            tuneLength = 5, trControl = fitControl, metric = "ROC")
    } else if (model_name == "gbm") {
        model_fit <- caret::train(
            y ~ ., data = logcpm_table, method = "gbm",
            trControl = fitControl, tuneLength = 5, metric = "ROC", verbose = FALSE)
    } else if (model_name == "random forest") {
        model_fit <- caret::train(
            y ~ ., data = logcpm_table, method = "ranger", trControl = fitControl,
            tuneLength = 5, metric = "ROC", importance = "impurity")
    }
    mod_pred <- model_fit$pred |> tibble::as_tibble()
    prob_pred <- as.numeric(mod_pred$obs)
    prob_pred[prob_pred == 1] <- 0
    prob_pred[prob_pred == 2] <- 1
    
    neg_pos <- mod_pred |> dplyr::pull(dplyr::all_of(levels(model_fit$pred$obs)[2]))
    df_roc <- tibble::tibble(m = neg_pos, d = prob_pred)
    
    this_rocit <- ROCit::rocit(df_roc$m, df_roc$d)
    this_label <- paste("AUC =", round(this_rocit$AUC, 4))
    
    plot_dat <- tibble::tibble(FPR = this_rocit$FPR, TPR = this_rocit$TPR)
    
    roc_plot <- ggplot2::ggplot(data = plot_dat, ggplot2::aes(x = 'FPR', y = 'TPR')) +
        ggplot2::geom_line(ggplot2::aes_string(x = 'FPR', y = 'TPR', col = base::shQuote("Empirical ROC curve"))) +
        ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1, col = "Chance line"), linewidth = 1,
            linetype = "dashed", show.legend = FALSE, alpha = 0.5) +
        ggplot2::labs(x = "1-Specificity (FPR)", y = "Sensitivity (TPR)",
            title = "Emperical ROC Curve", subtitle = this_label) +
        ggplot2::scale_color_manual("",
            labels = c("Chance line", "Empirical ROC curve"),
            values = c("grey", "red")) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    
    biomarker <- data.frame(biomarker_list = biomarker)
    # output a list
    list_output <- list(
        biomarker = biomarker,
        importance_plot = importance_plot,
        roc_plot = roc_plot
    )
    return(list_output)
}
