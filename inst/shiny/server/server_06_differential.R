
  run.deseq2 <- eventReactive(input$run_deseq2, {
    MAE = vals$MAE
    differential_abundance(MAE = MAE,
                          tax_level=input$taxl.da,
                          input_da_condition=input$da_condition,
                          input_da_condition_covariate=input$da_condition_covariate,
                          min_num_filter = input$da.count.cutoff,
                          input_da_padj_cutoff = input$da.padj.cutoff)

  })


  output$DeSeq2Table.new <- DT::renderDataTable({
    run.deseq2()
  },
  options=dtopts)

