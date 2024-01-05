
## plot alpha diversity
plotAlphaBoxplotButton <- eventReactive(input$alpha_boxplot,{
  alpha_div_boxplot(MAE = vals$MAE,
                  tax_level = input$taxl.alpha,
                  condition = input$select_alpha_div_condition,
                  alpha_metric = input$select_alpha_div_method)
})
output$AlphaDiversity <- renderPlotly({
  plotAlphaBoxplotButton()
})


#
# # Alpha diversity table
# do_alpha_table <- function() {
#   shinyInput <- vals$shiny.input
#   physeq1 <- shinyInput$pstat
#   if (input$taxl.alpha !="no rank")  {
#     physeq1 <- tax_glom(physeq1, input$taxl.alpha)
#   }
#   meta.data <- physeq1@sam_data
#   meta.data$sample.name <- rownames(meta.data)
#   meta.data$richness <- suppressWarnings(estimate_richness(physeq = physeq1, split = T, measures = input$select_alpha_div_method)[,1])
#   colnames(meta.data)[which(colnames(meta.data) == input$select_alpha_div_condition)] <- "condition"
#   rownames(meta.data) <- seq_len(nrow(meta.data))
#   DT::datatable(meta.data %>% dplyr::select(sample.name, condition, richness))
# }
#
#
# plotAlphaBoxplotButton2 <- eventReactive(input$alpha_boxplot,{
#   do_alpha_table()
# })
# output$table.alpha <- DT::renderDataTable({
#   plotAlphaBoxplotButton2()
# })
#
# # Download alpha diversity table
# output$download_table_alpha <- downloadHandler(
#   filename = function() { paste('Alpha_diversity_table', '.csv', sep='') },
#   content = function(file) {
#     shinyInput <- vals$shiny.input
#     physeq1 <- shinyInput$pstat
#     if (input$taxl.alpha !="no rank")  {
#       physeq1 <- tax_glom(physeq1, input$taxl.alpha)
#     }
#     meta.data <- physeq1@sam_data
#     meta.data$sample.name <- rownames(meta.data)
#     meta.data$richness <- suppressWarnings(estimate_richness(physeq = physeq1, split = T, measures = input$select_alpha_div_method)[,1])
#     colnames(meta.data)[which(colnames(meta.data) == input$select_alpha_div_condition)] <- "condition"
#     rownames(meta.data) <- seq_len(nrow(meta.data))
#     meta.data <- as_tibble(meta.data)
#     meta.data <- meta.data %>% select(sample.name, condition, richness)
#     write.csv(data.frame(meta.data), file)
#   }
# )

## do alpha diversity statistical test
plotAlphaBoxplotButton3 <- eventReactive(input$alpha_boxplot,{
  do_alpha_div_test(MAE = vals$MAE,
                  tax_level = input$taxl.alpha,
                  condition = input$select_alpha_div_condition,
                  alpha_metric = input$select_alpha_div_method,
                  alpha_stat = input$select_alpha_stat_method)
})
output$alpha.stat.test <- DT::renderDataTable({
  plotAlphaBoxplotButton3()
}, options = list(paging = TRUE, 
                  scrollX = TRUE, 
                  pageLength = 5,
                  sDom  = '<"top">t<"bottom">ip'))


## beta diversity heatmap
plotBetaHeatmapServerButton <- eventReactive(input$beta_heatmap,{
  diversity_beta_heatmap(MAE = vals$MAE,
                       tax_level = input$taxl.beta,
                       input_beta_method = input$beta_method,
                       input_bdhm_select_conditions = input$bdhm_select_conditions,
                       input_bdhm_sort_by = input$bdhm_sort_by)
})
output$BetaDiversityHeatmap <- renderPlotly({
  plotBetaHeatmapServerButton()
})


## beta diversity boxplot
plotBetaBoxplotServerButton <- eventReactive(input$beta_boxplot,{
  diversity_beta_boxplot(MAE = vals$MAE,
                       tax_level = input$taxl.beta,
                       input_beta_method = input$beta_method,
                       input_select_beta_condition = input$select_beta_condition)
})
output$BetaDiversityBoxplot <- renderPlotly({
  plotBetaBoxplotServerButton()
})

plotBetaBoxplotServerButton3 <- eventReactive(input$beta_boxplot, {
  diversity_beta_test(MAE = vals$MAE,
                    tax_level = input$taxl.beta,
                    input_beta_method = input$beta_method,
                    input_select_beta_condition = input$select_beta_condition,
                    input_select_beta_stat_method = input$select_beta_stat_method,
                    input_num_permutation_permanova = input$num_permutation_permanova)
})
output$beta.stat.test <- DT::renderDataTable({
  plotBetaBoxplotServerButton3()
}, options = list(sDom  = '<"top">t<"bottom">ip'))

plotBetaNMDSServerButton <- eventReactice(input$beta_NMDS, {
    #run R code for NMDS
})

output$betaDiveristyNMDSPlot <- renderPlotly({
    plotBetaNMDSServerButton()
})
