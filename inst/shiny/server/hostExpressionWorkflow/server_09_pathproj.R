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
do_path_proj <- eventReactive(input$path_proj_btn, {
  withBusyIndicatorServer("path_proj_btn", {
    
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
    
  })
  vals$MAE <- suppressWarnings(insert_into_MAE(vals$MAE, gsva_data))
  update_inputs(session)
  return(p)
})

output$score_heatmap <- renderPlotly({
  p <- do_path_proj()
  return(p)
})