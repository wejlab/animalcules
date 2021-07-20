#
# GSVA
# 

# When to display sub-collection options to user
output$subCollect <- renderUI({
  if(input$collect == 'C2'){
    x <- sub_collections['C2'][[1]]
  }else if(input$collect == 'C3'){
    x <- sub_collections['C3'][[1]]
  } else if(input$collect == 'C4'){
    x <- sub_collections['C4'][[1]]
  } else if(input$collect == 'C5'){
    x <- sub_collections['C5'][[1]]
  } else{
    x <- "No sub-collections available"
  }
  selectInput("subC", "Select a sub-collection:", choices = x)
})

# Pathway projection
observeEvent(input$path_proj_btn, {
  withBusyIndicatorServer("path_proj_btn", {
    # Doing pathway projection
    if(input$subC == "No sub-collections available"){
      gsva_data <- suppressWarnings(path_proj(MAE = vals$MAE,
                                              collection = input$collect,
                                              sub_collection = NA))
    } else {
      # browser()
      gsva_data <- suppressWarnings(path_proj(MAE = vals$MAE,
                                              collection = input$collect,
                                              sub_collection = input$subC)) 
    }
    p <- plotly::plot_ly(x=colnames(assays(gsva_data)[[1]]),
                         y=rownames(assays(gsva_data)[[1]]),
                         z=assays(gsva_data)[[1]], 
                         type = "heatmap",
                         colors = "Blues")
    output$score_heatmap <- renderPlotly({p})
    
    # Storing gsva scores in MAE
    vals$MAE <- suppressWarnings(insert_into_MAE(vals$MAE, gsva_data))
    
    # Updating inputs
    update_inputs(session)
  })
})