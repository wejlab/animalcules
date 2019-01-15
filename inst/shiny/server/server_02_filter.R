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

# Filter by metadata
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
        update_inputs(session)
    })
})

# Filter by microbes
#observeEvent(input$filter_microbes_read_btn,{
#    withBusyIndicatorServer("filter_microbes_read_btn", {
#        
#        MAE <- vals$MAE
#        microbe <- MAE[['MicrobeGenetics']]
#        counts_table <- as.data.frame(assays(microbe)) # organism x sample
#        
#        # Filter by average read number
#        minval <- input$filter_microbes_read_inp
#        means <- rowMeans(counts_table)
#        organisms <- names(means[means >= minval])
#    })
#})






# Discard Samples
observeEvent(input$filter_sample_dis_btn,{
    withBusyIndicatorServer("filter_sample_dis_btn", {
        vals$MAE <- mae_pick_samples(MAE = vals$MAE, discard_samples = input$filter_sample_dis)
        update_inputs(session)
    })
})

# Reset data
observeEvent(input$filter_reset_btn,{
    withBusyIndicatorServer("filter_reset_btn", {
        vals$MAE <- vals$MAE_backup
        update_inputs(session)
    })
})

output$filter_summary_top_plot <- renderPlotly({
    p <- filter_summary_top(MAE = vals$MAE,
                            samples_discard = c(),
                            filter_type = input$filter_type,
                            sample_condition = input$filter_type_metadata)
    return(p)
})

output$filter_summary_bottom_plot <- renderPlotly({
    p <- filter_summary_bottom(MAE = vals$MAE,
                               samples_discard = c(),
                               filter_type = input$filter_type,
                               sample_condition = input$filter_type_metadata)
    return(p)
})







# Categorize
output$filter_nbins <- renderUI({
    MAE <- vals$MAE
    microbe <- MAE[['MicrobeGenetics']]
    sam_table <- as.data.frame(colData(microbe)) # sample x condition
    vals <- unlist(sam_table[,input$filter_bin_cov,drop=TRUE])
    sliderInput("filter_nbins", label="Number of Bins", min=2, max=length(unique(vals)), value=2, step=1)
})
output$filter_bin_to1 <- renderPrint({
    x <- sort(as.numeric(unlist(strsplit(input$filter_bin_breaks,","))))
    print(x)
})
output$filter_bin_to2 <- renderPrint({
    x <- unlist(strsplit(input$filter_bin_labels,","))
    print(x)
})

output$filter_unbin_plot <- renderPlotly({
    MAE <- vals$MAE
    microbe <- MultiAssayExperiment::experiments(MAE)[[1]]
    samples <- as.data.frame(colData(microbe))
    result <- filter_categorize(samples,
                                sample_condition = input$filter_bin_cov,
                                new_label = input$filter_new_covariate)

    return(result$plot.unbinned)
})

do_categorize <- eventReactive(input$filter_create_bins, {
    MAE <- vals$MAE
    microbe <- MultiAssayExperiment::experiments(MAE)[[1]]
    samples <- as.data.frame(colData(microbe))

    nbins <- input$filter_nbins
    n <- input$filter_nbins

    # Overide custom bins if specified
    bin_breaks = sort(as.numeric(unlist(strsplit(input$filter_bin_breaks,","))))
    if (length(bin_breaks) > 1) {
      nbins = bin_breaks
      n = length(bin_breaks)-1
    }
    
    # Add custom labels only if the correct amount is sepecified
    bin_labels = unlist(strsplit(input$filter_bin_labels,","))
    fx_labels <- NULL
    if (length(bin_labels) == n) {
      fx_labels <- bin_labels
    }

    result <- filter_categorize(samples,
                                sample_condition = input$filter_bin_cov,
                                new_label = input$filter_new_covariate,
                                nbins = nbins,
                                bin_breaks = bin_breaks,
                                bin_labels = fx_labels)  

    # Modify sample table
    MAE@colData <- DataFrame(result$sam_table)
    vals$MAE <- MAE
    update_inputs(session)

    return(result$plot.binned)
})

# Reaction to button pressing
output$filter_bin_plot <- renderPlotly({
    p <- do_categorize()
    return(p)
})


