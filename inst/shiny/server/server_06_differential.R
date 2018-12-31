
  run.deseq2 <- eventReactive(input$run_deseq2, {
    MAE = vals$MAE

    target.var.index <- which(colnames(colData(MAE)) == input$da_condition)
    label.vec.num <- colData(MAE)[[target.var.index]]

    # if selected condition has multiple levels
    if (length(input$da_condition_options_use) == 2){
      sample.keep.index <- which(label.vec.num %in% input$da_condition_options_use)
      label.vec.num <- label.vec.num[sample.keep.index]
      MAE <- MAE[, label.vec.num %in% input$da_condition_options_use, ]
    }


   differential_abundance(MAE = MAE,
                          tax_level=input$taxl.da,
                          input_da_condition=input$da_condition,
                          input_da_condition_covariate=input$da_condition.covariate,
                          min_num_filter = input$da.count.cutoff,
                          input_da_padj_cutoff = input$da.padj.cutoff)

  })


  output$DeSeq2Table.new <- DT::renderDataTable({
    run.deseq2()
  },
  options = list(
      paging = TRUE
  ))




  ### check whether selected condition for DA has two levels or more
  output$da_condition_type <- reactive({
    MAE = vals$MAE

    covariates = colnames(colData(MAE))

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


    target.var.index <- which(colnames(colData(MAE)) == input$da_condition)
    if (is.integer0(target.var.index)){
      target.var.index <- 1
    }
    label.vec <- colData(MAE)[[target.var.index]]
    label.level.num <- length(unique(label.vec))
    factor.non.categorical <- covariates[which(!covariates %in% covariates.colorbar)]
    if (label.level.num == 2 || input$da_condition %in% factor.non.categorical){
      return("binary")
    } else{
      return("multiple")
    }

  })
  outputOptions(output, "da_condition_type", suspendWhenHidden = FALSE)

  # select 2 levels
  output$da_condition_options <- renderUI({
    MAE = vals$MAE
    variable.vec <- colData(MAE)[[
      which(colnames(colData(MAE)) == input$da_condition)]]
    filter.option.vec <- sort(unique(variable.vec))
    tagList(
      selectInput("da_condition_options_use", "Select 2 levels", choices = filter.option.vec, multiple = TRUE)
    )
  })
t
