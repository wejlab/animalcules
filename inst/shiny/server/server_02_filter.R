output$filter_metadata_params <- renderUI({

    MAE <- vals$MAE
    microbe <- MAE[['MicrobeGenetics']]
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    covdat <- sam_table[,input$filter_type_metadata]    

    if (!is.categorical(covdat)) {
        sliderInput("filter_metadata_inp", "Include", min = min(covdat), max = max(covdat), value = c(min(covdat), max(covdat)))
    } else {
        selectizeInput("filter_metadata_inp", "Include", choices=unique(covdat), selected=unique(covdat), multiple=TRUE)
    }
})

observeEvent(input$filter_metadata_btn,{
    withBusyIndicatorServer("filter_metadata_btn", {
        
        MAE <- vals$MAE
        microbe <- MAE[['MicrobeGenetics']]
        sam_table <- as.data.frame(colData(microbe)) # sample x condition
        cov <- input$filter_type_metadata
        covdat <- sam_table[,cov]

        if (!is.categorical(covdat)) {
            minval <- input$filter_metadata_inp[1]
            maxval <- input$filter_metadata_inp[2]
            sam_table <- sam_table[covdat >= minval & covdat <= maxval,,drop=FALSE]
        } else {
            include <- input$filter_metadata_inp
            sam_table <- sam_table[covdat %in% include,,drop=FALSE]
        }
        samples <- rownames(sam_table)
        vals$MAE <- mae_pick_samples(MAE = vals$MAE, isolate_samples = samples)
    })
})


output$filter_summary_top_plot <- renderPlotly({
    p <- filter_summary_top(MAE = vals$MAE,
                            samples_discard = input$filter_sample_dis,
                            filter_type = input$filter_type,
                            sample_condition = input$filter_type_metadata)
    return(p)
})

output$filter_summary_bottom_plot <- renderPlotly({
    p <- filter_summary_bottom(MAE = vals$MAE,
                               samples_discard = input$filter_sample_dis,
                               filter_type = input$filter_type,
                               sample_condition = input$filter_type_metadata)
    return(p)
})