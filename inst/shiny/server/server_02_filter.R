output$filter_sample_cov_params <- renderUI({

    # Subset the data
    MAE_subset <- mae_pick_samples(MAE = vals$MAE, discard_samples = input$filter_sample_dis)
    microbe <- MAE_subset[['MicrobeGenetics']]
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    covdat = sam_table[,input$filter_sample_cov]    

    if (!is.categorical(covdat)) {
        sliderInput("filter_sample_cov_pick", "Include", min = min(covdat), max = max(covdat), value = c(min(covdat), max(covdat)))
    } else {
        selectizeInput("filter_sample_cov_pick", "Include", choices=unique(covdat), selected=unique(covdat), multiple=TRUE)
    }
})


output$filter_summary_top_plot <- renderPlotly({
    p <- filter_summary_top(MAE = vals$MAE,
                            samples_discard = input$filter_sample_dis,
                            filter_type = input$filter_type,
                            sample_condition = input$filter_sample_cov)
    return(p)
})

output$filter_summary_bottom_plot <- renderPlotly({
    p <- filter_summary_bottom(MAE = vals$MAE,
                               samples_discard = input$filter_sample_dis,
                               filter_type = input$filter_type,
                               sample_condition = input$filter_sample_cov)
    return(p)
})