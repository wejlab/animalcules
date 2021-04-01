#
# GSVA
# 
# sub-collection
pathred <- reactiveValues(sumexp = NULL,
                          data = NULL)

output$subCollect <- renderUI({
  if(input$collect == 'C2 = Curated'){
    x <- sub_collections['C2'][[1]]
  }else if(input$collect == 'C3 = Regulatory Targets'){
    x <- sub_collections['C3'][[1]]
  } else if(input$collect == 'C4 = Computational'){
    x <- sub_collections['C4'][[1]]
  } else if(input$collect == 'C5 = Ontology'){
    x <- sub_collections['C5'][[1]]
  } else{
    x <- NA
  }
  selectInput("subC", "Select a sub-collection:", choices = x)
})

observeEvent(input$dimred_gsva_btn, {
  withBusyIndicatorServer("dimred_gsva_btn", {
    if(is.na(input$subC)){
      result <- genes_dimred_gsva(MAE = vals$MAE,
                                  collection = input$collection)
      pathred$sumexp <- result
    } else{
      result <- genes_dimred_gsva(MAE = vals$MAE,
                                  collection = input$collection,
                                  sub_collection = input$subC)
      pathred$sumexp <- result
    }
    pathred$data <- assays(pathred$sumexp)[[1]]
  })
})

# do_dimred_gsva <- eventReactive(input$dimred_gsva_btn, {
#   withBusyIndicatorServer("dimred_gsva_btn", {
#     if(is.na(input$subCollect)){
#       result <- genes_dimred_gsva(MAE = vals$MAE,
#                                   collection = input$collection)
#       vals$genes_gsva <- result
#     } else{
#       result <- genes_dimred_gsva(MAE = vals$MAE,
#                                   collection = input$collection,
#                                   sub_collection = input$subCollect)
#       vals$genes_gsva <- result
#     }
#     return(result)
#   })
# })
output$genes_dimred_gsva <- renderDataTable({datatable(pathred$data)})