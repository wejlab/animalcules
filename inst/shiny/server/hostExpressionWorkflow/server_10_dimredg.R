#
# PCA
#
# Plot
do_dimredg_pca_plot <- eventReactive(input$dimredg_pca_plot_btn, {
  withBusyIndicatorServer("dimredg_pca_plot_btn", {
    if (input$dimredg_pca_shape == "None") {shape <- NULL} else {shape <- input$dimredg_pca_shape}
    if (is.na(input$dimredg_pca_z)) {pcz <- NULL} else {pcz <- input$dimredg_pca_z}
    result <- dimredg_pca(MAE = vals$MAE,
                          color = input$dimredg_pca_color,
                          shape = shape,
                          pcx = input$dimredg_pca_x,
                          pcy = input$dimredg_pca_y,
                          pcz = pcz,
                          datatype = input$dimredg_pca_datatype)
    return(suppressWarnings(result$plot))
  })
})
output$dimredg_pca_plot <- renderPlotly({
  p <- suppressWarnings(do_dimredg_pca_plot())
  return(suppressWarnings(p))
})
# Table
do_dimredg_pca_table <- eventReactive(input$dimredg_pca_table_btn, {
  if (input$dimredg_pca_shape == "None") {shape <- NULL} else {shape <- input$dimredg_pca_shape}
  result <- dimredg_pca(MAE = vals$MAE,
                       color = input$dimredg_pca_color,
                       shape = shape,
                       pcx = input$dimredg_pca_x,
                       pcy = input$dimredg_pca_y,
                       datatype = input$dimredg_pca_datatype)
  return(result$table)
})
output$dimredg_pca_table <- renderDataTable({
  t <- do_dimredg_pca_table()
  DT::datatable(t, 
                rownames = FALSE,
                options = list(paging=TRUE, 
                               pageLength=15, 
                               searching=FALSE, 
                               lengthChange=FALSE))
})

#
# PCoA
#
# Plot
# do_dimredg_pcoa_plot <- eventReactive(input$dimredg_pcoa_plot_btn, {
#   if (input$dimredg_pcoa_shape == "None") {shape <- NULL} else {shape <- input$dimredg_pcoa_shape}
#   if (is.na(input$dimredg_pcoa_z)) {axz <- NULL} else {axz <- input$dimredg_pcoa_z}
#   result <- dimredg_pcoa(MAE = vals$MAE,
#                          color = input$dimredg_pcoa_color,
#                          shape = shape,
#                          axx = input$dimredg_pcoa_x,
#                          axy = input$dimredg_pcoa_y,
#                          axz = axz,
#                          method = input$dimredg_pcoa_method)
#   return(result$plot)
# })
# output$dimredg_pcoa_plot <- renderPlotly({
#   p <- do_dimredg_pcoa_plot()
#   return(p)
# })
# # Table
# do_dimredg_pcoa_table <- eventReactive(input$dimredg_pcoa_table_btn, {
#   if (input$dimredg_pcoa_shape == "None") {shape <- NULL} else {shape <- input$dimredg_pcoa_shape}
#   result <- dimredg_pcoa(MAE = vals$MAE,
#                          color = input$dimredg_pcoa_color,
#                          shape = shape,
#                          axx = input$dimredg_pcoa_x,
#                          axy = input$dimredg_pcoa_y,
#                          method = input$dimredg_pcoa_method)
#   return(result$table)
# })
# output$dimredg_pcoa_table <- renderDataTable({
#   t <- do_dimredg_pcoa_table()
#   DT::datatable(t,
#                 rownames = FALSE,
#                 options = list(paging=TRUE,
#                                pageLength=15,
#                                searching=FALSE,
#                                lengthChange=FALSE))
# })

#
# UMAP
#
# Plot
do_dimredg_umap_plot <- eventReactive(input$dimredg_umap_plot_btn, {
  if (input$dimredg_umap_shape == "None") {shape <- NULL} else {shape <- input$dimredg_umap_shape}
  if (is.na(input$dimredg_umap_z)) {cz <- NULL} else {cz <- input$dimredg_umap_z}
  result <- dimredg_umap(MAE = vals$MAE,
                         color = input$dimredg_umap_color,
                         shape = shape,
                         cx = input$dimredg_umap_x,
                         cy = input$dimredg_umap_y,
                         cz = cz,
                         n_neighbors = input$dimredg_umap_n_neighbors,
                         metric = input$dimredg_umap_metric,
                         n_epochs = input$dimredg_umap_n_epochs,
                         init = input$dimredg_umap_init,
                         min_dist = input$dimredg_umap_min_dist,
                         datatype = input$dimredg_umap_datatype)
  return(suppressWarnings(result$plot))
})
output$dimredg_umap_plot <- renderPlotly({
  p <- suppressWarnings(do_dimredg_umap_plot())
  return(suppressWarnings(p))
})

# #
# # t-SNE
# #
# # Plot
do_dimredg_tsne_plot <- eventReactive(input$dimredg_tsne_plot_btn, {
  withBusyIndicatorServer("dimredg_tsne_plot_btn", {
    if (input$dimredg_tsne_shape == "None") {shape <- NULL} else {shape <- input$dimredg_tsne_shape}
    if (input$dimredg_tsne_cached & !is.null(vals$tsne)) {
      # Used cached t-SNE results
      result <- dimredg_tsne(MAE = vals$MAE,
                            color = input$dimredg_tsne_color,
                            shape = shape,
                            tsne_cache = vals$tsne$data)
    } else {
      result <- dimredg_tsne(MAE = vals$MAE,
                            color = input$dimredg_tsne_color,
                            shape = shape,
                            k = input$dimredg_tsne_k,
                            initial_dims= input$dimredg_tsne_initial_dims,
                            perplexity = input$dimredg_tsne_perplexity)
    }
    vals$tsne <- result # Cache results
    return(result$plot)
  })
})
output$dimredg_tsne_plot <- renderPlotly({
  p <- do_dimredg_tsne_plot()
  return(p)
})
