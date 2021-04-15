# Correlation matrix
# Setting placeholders

#source("/Users/saketpandit/Documents/BU/Johnson_lab/Asthma/Scripts/corr_func.R")
# runCorr action
# observeEvent(input$runCorr, {
#   cormat$data <- corr_func(MAE, asys = c(input$assay1, input$assay2))
#   for(microbe in rownames(cormat$data)){
#     gene_num <- length(names(which(cormat$data[microbe,]>0)))
#     if(gene_num >= 20){
#       mics$list <- c(microbe, mics$list)
#     }
#   }
# })

summary_table <- reactiveValues(data = NULL)

observeEvent(input$do_corr_btn, {
  withBusyIndicatorServer("do_corr_btn", {
    if(input$assay1 == input$assay2){
      asy=c(input$assay1)
      tx.lvls <- c(input$tax.level1, input$tax.level2)
    } else{
      asy=c(input$assay1, input$assay2)
      tx.lvls <- input$tax.level1
    }
    if(input$dispOpt==FALSE){
      result <- suppressWarnings(corr_func(MAE = vals$MAE,
                                           asys = asy,
                                           no.sig = input$no.sig,
                                           tax_level = tx.lvls))
    } else{
      result <- suppressWarnings(corr_func(MAE = vals$MAE,
                                           asys = asy,
                                           no.sig = input$no.sig,
                                           tax_level = tx.lvls,
                                           hide_ax = input$axis_lab))
    }
    summary_table$data <- result$summary
    # Heat map
    output$corr_plot <- renderPlotly({result$plot})
    # Summary table
    output$corr_summary <- renderDataTable({DT::datatable(result$summary_t10)})
    })
  output$gList <- renderUI({
    if(input$assay2 == 'hostExpression'){
      selectInput("OTU", "Select Group for enrichR analysis:",
                  summary_table$data$OTU)
    } else {
      "Gene Expression data not selected"
    }
  })
})



# do_corr <- eventReactive(input$do_plot_btn, {
#   withBusyIndicatorServer("do_plot_btn", {
#     if(input$assay1 == input$assay2){
#       asy=c(input$assay1)
#       tx.lvls <- c(input$tax.level1, input$tax.level2)
#       }else{
#         asy=c(input$assay1, input$assay2)
#         tx.lvls <- input$tax.level1
#       }
#     if(input$dispOpt==FALSE){
#       result <- suppressWarnings(corr_func(MAE = vals$MAE,
#                                            asys = asy,
#                                            no.sig = input$no.sig,
#                                            tax_level = tx.lvls))
#     } else{
#       result <- suppressWarnings(corr_func(MAE = vals$MAE,
#                                            asys = asy,
#                                            no.sig = input$no.sig,
#                                            tax_level = tx.lvls,
#                                            hide_ax = input$axis_lab))
#     }
#     return(result)
#   })
# })
# 
# # Plotting heatmap
# output$corr_plot <- renderPlotly({
#   p <- do_corr()
#   p$plot
# })


# observeEvent(input$runCorr, {
#   withBusyIndicatorServer("runCorr", {
#     if(input$assay1 == input$assay2){
#       results <- corr_func(MAE = vals$MAE,
#                            asys = c(input$assay1),
#                            tax_level = c(input$tax.level1, input$tax.level2),
#                            no.sig = input$no.sig,
#                            hide_ax = input$axis_lab)
#     } else{
#       results <- corr_func(MAE = vals$MAE,
#                            asys = c(input$assay1, input$assay2),
#                            no.sig = input$no.sig,
#                            hide_ax = input$axis_lab)
#     }
#     res$plot <- results$plot
#     res$summary <- results$summary
#     res$cormat <- results$cormat
#   })
# })

# do_corr_summary <- eventReactive(input$do_summary_btn, {
#   withBusyIndicatorServer("do_summary_btn", {
#     if(input$assay1 == input$assay2){
#       as=c(input$assay1)
#       tx.lvls <- c(input$tax.level1, input$tax.level2)
#     }else{
#       as=c(input$assay1, input$assay2)
#       tx.lvls <- input$tax.level1
#     }
#     result <- suppressWarnings(corr_func(MAE = vals$MAE,
#                                          asys = as,
#                                          no.sig = input$no.sig,
#                                          tax_level = tx.lvls,
#                                          hide_ax = input$axis_lab))
#     
#     return(result$summary)
#   })
# })


# Display correlation matrix
#output$corr_summary <- renderDataTable({DT::datatable(tbl)})

# output$corr_summary <- renderDataTable({datatable(
#   res$summary#[order(results$summary$Group_Size, decreasing = TRUE),]
#   #cormat$summary[order(cormat$summary$Group_Size, decreasing = TRUE),]
# )})


# Enrichment Analysis

observeEvent(input$do_enrich_btn, {
  withBusyIndicatorServer("do_enrich_btn", {
    gsets <- hypeR::enrichr_gsets(input$db, db="Enrichr")
    signature <- summary_table$data %>% 
      dplyr::filter(OTU == input$OTU) %>% 
      dplyr::pull(Groups)
    signature <- strsplit(signature, split=";")[[1]]
    hyp <- hypeR::hypeR(signature, gsets, test="hypergeometric")
    p <- hypeR::hyp_dots(hyp, top=10, fdr=0.25)
    # These are just ggplot objects you could customize
    p <- p + theme(axis.text=element_text(size=12, face="bold"))
    output$enrichmentTable <- renderPlot({p})
  })
})


# output$mList <- renderUI({
#   if(input$assay2 == 'hostExpression'){
#     if(input$assay1 == "MicrobeGenetics"){
#       selectInput("OTU", "Select OTU for enrichR analysis:",
#                   result$summary$OTU)
#   } else if(input$assay1 == "hervAbundance"){
#     selectInput("HERV", "Select HERV for enrichR analysis:",
#                 result$summary$OTU)
#   }
#   } else {
#     "Gene Expression data not selected"
#   }
# })
# 
# # Setting placeholders
# glst <- reactiveValues(genes = NULL)
# enr <- reactiveValues(table = NULL)
# 
# observeEvent(input$enrich, {
#   glst$genes <- names(res$summary[input$OTU,])
#   enr$table <- enrichr(glst$genes, database = input$db)[[1]]
#   })
# 
# output$enrichmentTable <- renderDataTable({enr$table})
