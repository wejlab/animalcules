#
# PCA
#
# Plot
do_dimred_pca_plot <- eventReactive(input$dimred_pca_plot_btn, {
    if (input$dimred_pca_shape == "None") {shape <- NULL} else {shape <- input$dimred_pca_shape}
    result <- dimred_pca(MAE = vals$MAE,
                         tax_level = input$dimred_pca_taxlev,
                         color = input$dimred_pca_color,
                         shape = shape,
                         pcx = input$dimred_pca_x,
                         pcy = input$dimred_pca_y,
                         datatype = input$dimred_pca_datatype)
    return(result$plot)
})
output$dimred_pca_plot <- renderPlotly({
    p <- do_dimred_pca_plot()
    return(p)
})
# Table
do_dimred_pca_table <- eventReactive(input$dimred_pca_table_btn, {
    if (input$dimred_pca_shape == "None") {shape <- NULL} else {shape <- input$dimred_pca_shape}
    result <- dimred_pca(MAE = vals$MAE,
                         tax_level = input$dimred_pca_taxlev,
                         color = input$dimred_pca_color,
                         shape = shape,
                         pcx = input$dimred_pca_x,
                         pcy = input$dimred_pca_y,
                         datatype = input$dimred_pca_datatype)
    return(result$table)
})
output$dimred_pca_table <- renderDataTable({
    t <- do_dimred_pca_table()
    return(t)
})

#
# PCoA
#
# Plot
do_dimred_pcoa_plot <- eventReactive(input$dimred_pcoa_plot_btn, {
    if (input$dimred_pcoa_shape == "None") {shape <- NULL} else {shape <- input$dimred_pcoa_shape}
    result <- dimred_pcoa(MAE = vals$MAE,
                         tax_level = input$dimred_pcoa_taxlev,
                         color = input$dimred_pcoa_color,
                         shape = shape,
                         axx = input$dimred_pcoa_x,
                         axy = input$dimred_pcoa_y,
                         method = input$dimred_pcoa_datatype)
    return(result$plot)
})
output$dimred_pcoa_plot <- renderPlotly({
    p <- do_dimred_pcoa_plot()
    return(p)
})
# Table
do_dimred_pcoa_table <- eventReactive(input$dimred_pcoa_table_btn, {
    if (input$dimred_pcoa_shape == "None") {shape <- NULL} else {shape <- input$dimred_pcoa_shape}
    result <- dimred_pcoa(MAE = vals$MAE,
                          tax_level = input$dimred_pcoa_taxlev,
                          color = input$dimred_pcoa_color,
                          shape = shape,
                          axx = input$dimred_pcoa_x,
                          axy = input$dimred_pcoa_y,
                          method = input$dimred_pcoa_method)
    return(result$table)
})
output$dimred_pcoa_table <- renderDataTable({
    t <- do_dimred_pcoa_table()
    return(t)
})