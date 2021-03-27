# Correlation matrix
# Setting placeholders

#source("/Users/saketpandit/Documents/BU/Johnson_lab/Asthma/Scripts/corr_func.R")
cormat <- reactiveValues(data = NULL,
                         ns = NULL, os = NULL, gs = NULL,
                         summary = NULL,
                         plot = NULL)
mics <- reactiveValues(list = NULL)

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

observeEvent(input$runCorr, {
  withBusyIndicatorServer("runCorr", {
    if(input$assay1 == input$assay2){
      cormat$data <- corr_func(MAE = vals$MAE,
                               asys = c(input$assay1),
                               tax_level = c(input$tax.level1, input$tax.level2))
    } else {
      cormat$data <- corr_func(MAE = vals$MAE,
                               asys = c(input$assay1, input$assay2),
                               tax_level = c(input$tax.level1),
                               no.sig = input$no.sig)
    }
    cormat$ns <- c()
    cormat$os <- c()
    cormat$gs <- c()
    for(otu in rownames(cormat$data)){
      grp <- names(which(cormat$data[otu,]>0))
      num <- length(grp)
      if(num > 0){
        cormat$ns <- c(cormat$ns, num)
        cormat$os <- c(cormat$os, otu)

        grp <- paste0(grp, sep = ";", collapse = "")
        cormat$gs <- c(cormat$gs, grp)
      }
    }
    cormat$summary <- data.frame(OTU = cormat$os,
                                 Group_Size = cormat$ns,
                                 Group = cormat$gs)
    # cormat$plot <- plot_ly(x = colnames(cormat$data),
    #                     y = rownames(cormat$data),
    #                     z = cormat$data, type = "heatmap")
    if(input$axis_lab == "xax") {
      cormat$plot <- heatmaply(cormat$data,
                               showticklabels = c(FALSE, TRUE))
    } else if(input$axis_lab == "yax"){
      cormat$plot <- heatmaply(cormat$data,
                               showticklabels = c(TRUE, FALSE))
    } else if(input$axis_lab == "bax"){
      cormat$plot <- heatmaply(cormat$data,
                               showticklabels = c(FALSE, FALSE))
    } else{
      cormat$plot <- heatmaply(cormat$data)
    }

  })
})

# Display correlation matrix
output$corr_summary <- renderDataTable({datatable(
  cormat$summary[order(cormat$summary$Group_Size, decreasing = TRUE),]
)})

# Plotting heatmap
output$corr_plot <- renderPlotly({cormat$plot})

# Enrichment Analysis
output$mList <- renderUI({
  if(input$assay2 == 'hostExpression'){
    if(input$assay1 == "MicrobeGenetics"){
      selectInput("OTU", "Select OTU for enrichR analysis:",
                  cormat$os)
  } else if(input$assay1 == "hervAbundance"){
    selectInput("HERV", "Select HERV for enrichR analysis:",
                cormat$os)
  }
  } else {
    "Gene Expression data not selected"
  }
})

# Setting placeholders
glst <- reactiveValues(genes = NULL)
enr <- reactiveValues(table = NULL)

observeEvent(input$enrich, {
  glst$genes <- names(cormat$data[input$OTU,])
  enr$table <- enrichr(glst$genes, database = input$db)[[1]]
})

output$enrichmentTable <- renderDataTable({enr$table})
