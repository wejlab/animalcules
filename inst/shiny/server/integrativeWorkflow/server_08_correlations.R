
# Placeholders
data <- reactiveValues(summary = NULL,
                       cormat = NULL)
data_split <- reactiveValues(summary1 = NULL,
                             cormat1 = NULL,
                             summary2 = NULL,
                             cormat2 = NULL)


# Correlations Action Button --------------------------------------------------------

observeEvent(input$do_corr_btn, {
  withBusyIndicatorServer("do_corr_btn", {
    
    # Removing previous tabs
    removeTab(inputId = "corr",
              target = "Co-Occurence Networks")
    removeTab(inputId = "corr",
              target = "Enrichment")
    
    ## PERFORM CORRELATION ##
    
    # Setting parameters
    # Assays + taxonomy levels
    if(input$assay1 == input$assay2 && 
       !is.na(input$tax.level1) == !is.na(input$tax.level2)){ 
      as <- c(input$assay1)
      t <- c(input$tax.level1, input$tax.level2)
    } else if (!is.na(input$tax.level1)){
      as <- c(input$assay1, input$assay2)
      t <- c(input$tax.level1)
    } else if (!is.na(input$tax.level2)){
      as <- c(input$assay1, input$assay2)
      t <- c(input$tax.level2)
    }
    
    # Advanced correlation parameters 
    # no.sig, correction, alpha
    if(input$advOptions==F){
      #print("AdvOptions not selected")
      n <- 1
      c <- p.adjust.methods[4]
      al <- 0.05
    } else {
      n <- input$no.sig
      c <- input$correction
      al <- input$alpha
    }
    
    # corr_func: single
    if(input$separate == "No"){
      result <- suppressWarnings(corr_func(MAE = vals$MAE,
                                           asys = as,
                                           tax_level = t,
                                           no.sig = n,
                                           correction = c,
                                           alpha = al)) 
      
      ## RENDER SUMMARY TABLES ##
      
      # Summary table --> Correlations tab
      # output$corr_summary <- renderDataTable({DT::datatable(result$summary,
      #                                                       extensions = "Buttons",
      #                                                       options = list(
      #                                                         dom = 'Bfrtip',
      #                                                         buttons = c("copy", "csv"),
      #                                                         scrollX = T)
      #                                                       )}, server = TRUE) # server = TRUE --> makes the table interactive
      output$corr_summary <- renderDataTable({DT::datatable(result$summary,
                                                            extensions = c("Select", "Buttons"),
                                                            options = list(
                                                              select = list(style = 'os', items = 'row'),
                                                              dom = 'Blfrtip',
                                                              rowId = 0,
                                                              buttons = c('selectAll', 'selectNone', 'selectRows'),
                                                              scrollX = T),
                                                            selection = "none")}, server = F)
      
      ## Descriptive text
      output$corr_summary_instruct <- renderUI({
        HTML("
        <h2>Select rows to plot on heat map</h2>
        <p>Shift + Click to select contiguous rows, 
        Ctrl/Command + Click to select separate rows</p>
             ")
      })
      
      # Summary table --> Network tab
      output$corr_summary_network <- renderDataTable({DT::datatable(result$summary,
                                                                    options = list(scrollX = T)
                                                                    )}, server = TRUE)
      output$corr_summary_network_instruct <- renderUI({
        HTML("<h2>Select row to create network</h2>")
      })
      
      
      # Summary table --> Enrichment tab
      output$corr_summary_enrichment <- renderDataTable({DT::datatable(result$summary,
                                                                       options = list(scrollX = T)
                                                                       )}, server = TRUE)
      output$corr_summary_enrichment_instruct <- renderUI({
        HTML("<h2>Select a row to calculate enrichment for:</h2>")
      })
      
      # "Select all" button for summary table 
      # dt_proxy <- DT::dataTableProxy("corr_summary")
      # observeEvent(input$dt_sel, {
      #   if (isTRUE(input$dt_sel)) {
      #     DT::selectRows(dt_proxy, input$corr_summary_rows_all)
      #   } else {
      #     DT::selectRows(dt_proxy, NULL)
      #   }
      # })
      # "Selected Rows" text
      # output$selected_rows <- renderPrint(print(input$corr_summary_rows_selected))

      # Storing results for later
      data$summary <- result$summary
      data$cormat <- result$cormat
      
      # # group list for network analysis
      # output$gList_network_single <- renderUI({
      #   selectInput("networkOTU_single", "Create co-occurence network for which Group?",
      #               result$summary$OTU)
      #   })
      
      # group list for enrichment analysis
      # output$gList_enrich_single <- renderUI({ 
      #   selectInput("enrichOTU_single", "Select Group for enrichR analysis:",
      #               result$summary$OTU)
      # })
      
      # corr_func: separate
      } else if (input$separate == "Yes") { 
        
        ## PERFORMING CORRELATION CALCULATION ##
        
        # Splitting into two conditions
        # %<-% comes from `zeallot` package
        c(con1, con2) %<-% separate_by_condition(vals$MAE, input$select_correlation_condition)
      
        # Condition 1: Results 
        result_con1 <- suppressWarnings(corr_func(MAE = con1,
                                                  asys = as,
                                                  tax_level = t,
                                                  no.sig = n,
                                                  correction = c,
                                                  alpha = al))
        
        # Condition 2: Results
        result_con2 <- suppressWarnings(corr_func(MAE = con2,
                                                  asys = as,
                                                  tax_level = t,
                                                  no.sig = n,
                                                  correction = c,
                                                  alpha = al))
        
        
        ## RENDERING SUMMARY TABLES ##
        
        # For correlations tab
        # Summary table for condition 1
        c1 <- paste0('Summary for condition: "', colData(con1)[input$select_correlation_condition][1,1], '"', 
                     collapse = "")
        output$corr_summary1 <- renderDataTable({DT::datatable(result_con1$summary,
                                                               caption = c1,
                                                               extensions = c("Select", "Buttons"),
                                                               options = list(
                                                                 select = list(style = 'os', items = 'row'),
                                                                 dom = 'Blfrtip',
                                                                 rowId = 0,
                                                                 buttons = c('selectAll', 'selectNone', 'selectRows'),
                                                                 scrollX = T),
                                                               selection = "none")}, server = F)
        # "Select all" button 
        # dt_proxy1 <- DT::dataTableProxy("corr_summary1")
        # observeEvent(input$dt_sel1, {
        #   if (isTRUE(input$dt_sel1)) {
        #     DT::selectRows(dt_proxy1, input$corr_summary1_rows_all)
        #   } else {
        #     DT::selectRows(dt_proxy1, NULL)
        #   }
        # })
        # # "Selected Rows" text
        # output$selected_rows1 <- renderPrint(print(input$corr_summary1_rows_selected))
        
        
        # Summary table for condition 2
        c2 <- paste0('Summary for condition: "', colData(con2)[input$select_correlation_condition][1,1], '"', 
                     collapse = "")
        output$corr_summary2 <- renderDataTable({DT::datatable(result_con2$summary,
                                                               caption = c2,
                                                               extensions = c("Select", "Buttons"),
                                                               options = list(
                                                                 select = list(style = 'os', items = 'row'),
                                                                 dom = 'Blfrtip',
                                                                 rowId = 0,
                                                                 buttons = c('selectAll', 'selectNone', 'selectRows'),
                                                                 scrollX = T),
                                                               selection = "none")}, server = F)
        # "Select all" button
        # dt_proxy2 <- DT::dataTableProxy("corr_summary2")
        # observeEvent(input$dt_sel2, {
        #   if (isTRUE(input$dt_sel2)) {
        #     DT::selectRows(dt_proxy2, input$corr_summary2_rows_all)
        #   } else {
        #     DT::selectRows(dt_proxy2, NULL)
        #   }
        # })
        # # "Selected Rows" text
        # output$selected_rows2 <- renderPrint(print(input$corr_summary2_rows_selected))
        output$corr_summary_split_instruct <- renderUI({
          HTML("
        <h2>Select rows to plot on heat map</h2>
        <p>Shift + Click to select contiguous rows, 
        Ctrl/Command + Click to select separate rows</p>
             ")
        })
        

        # For Co-Occurence Networks
        # Condition 1:
        output$corr_summary_network1 <- renderDataTable({DT::datatable(result_con1$summary,
                                                                       caption = c1,
                                                                       options = list(scrollX = T)
                                                                       )}, server = TRUE)
        # Condition 2: 
        output$corr_summary_network2 <- renderDataTable({DT::datatable(result_con2$summary,
                                                                       caption = c2,
                                                                       options = list(scrollX = T)
                                                                       )}, server = TRUE)
        output$corr_summary_network_split_instruct <- renderUI({
          HTML("<h2>Select one row from each table to create network</h2>")
        })
        
        
        # For Enrichment
        output$corr_summary_enrichment1 <- renderDataTable({DT::datatable(result_con1$summary,
                                                                          caption = c1,
                                                                          options = list(scrollX = T)
                                                                          )}, server = TRUE)
        output$corr_summary_enrichment_instruct1 <- renderUI({
          HTML("<h2>Select a row to calculate enrichment for</h2>")
        })
        
        output$corr_summary_enrichment2 <- renderDataTable({DT::datatable(result_con2$summary,
                                                                          caption = c2,
                                                                          options = list(scrollX = T)
                                                                          )}, server = TRUE)
        output$corr_summary_enrichment_instruct2 <- renderUI({
          HTML("<h2>Select a row to calculate enrichment for</h2>")
          })
        
        
        # Storing results for later
        data_split$summary1 <- result_con1$summary
        data_split$cormat1 <- result_con1$cormat
        data_split$summary2 <- result_con2$summary
        data_split$cormat2 <- result_con2$cormat
        
        # # Enrichment lists
        # output$gList_enrich_split1 <- renderUI({
        #   selectInput("enrichOTU_split1", "Select Group 1 for enrichR analysis:",
        #               result_con1$summary$OTU)
        #   })
        # 
        # output$gList_enrich_split2 <- renderUI({
        #   selectInput("enrichOTU_split2", "Select Group 2 for enrichR analysis:",
        #               result_con2$summary$OTU)
        #   })
        # 
        # # Network lists
        # output$gList_split1 <- renderUI({
        #   selectInput("networkOTU_split1", "Select Group 1 for network analysis:",
        #               result_con1$summary$OTU)
        #   })
        # 
        # output$gList_split2 <- renderUI({
        #   selectInput("networkOTU_split2", "Select Group 2 for network analysis:",
        #               result_con2$summary$OTU)
        #   })
        
        
      }
    
    ## DYANMICALLY DISPLAYING TABS (UI) ##

    # Co-occurence Network UI -------------------------------------------------
    if(exists("result") | exists("result_con1")) {
      appendTab(inputId = "corr",
                tabPanel("Co-Occurence Networks",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               checkboxInput("advOpt_network", "Advanced Network Options",
                                             value = FALSE),
                               conditionalPanel(condition = "input.advOpt_network == true",
                                                selectInput("p.adjust.network", "Select p-value correction method",
                                                            choices = c("sig", stats::p.adjust.methods), selected = "fdr"),
                                                numericInput("alpha_network", "Alpha:",
                                                             value = 0.05, min = 0.0001, max = 0.1, step = 0.0001)),
                               withBusyIndicatorUI(actionButton("do_network_btn", "Plot Network"))
                             ),
                             mainPanel(
                               conditionalPanel(condition = "input.separate == 'No'",
                                                fluidRow(
                                                  uiOutput("corr_summary_network_instruct"),
                                                  column(12, dataTableOutput("corr_summary_network"))
                                                ),
                                                fluidRow(
                                                  column(12,
                                                         plotOutput("corrNetwork_single"))
                                                )
                               ),
                               conditionalPanel(condition = "input.separate == 'Yes'",
                                                fluidRow(
                                                  uiOutput("corr_summary_network_split_instruct"),
                                                  column(6, dataTableOutput("corr_summary_network1")),
                                                  column(6, dataTableOutput("corr_summary_network2"))
                                                  ),
                                                fluidRow(
                                                  column(6,
                                                         plotOutput("corrNetwork_split1")),
                                                  column(6,
                                                         plotOutput("corrNetwork_split2"))
                                                  )
                               )
                             )
                           )
                         )
                )
                )
    }
    

    # Enrichment UI -------------------------------------------------------
    if(input$assay2 == "hostExpression") {
      appendTab(inputId = "corr",
                tabPanel("Enrichment",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("db", "Select database for enrichR:",
                                           dbList
                               ),
                               withBusyIndicatorUI(
                                 actionButton("do_enrich_btn", "Calculate Enrichment", class = "btn-primary")
                               ),
                             ),
                             mainPanel(
                               conditionalPanel(condition = 'input.separate == "No"',
                                                fluidRow(
                                                  uiOutput("corr_summary_enrichment_instruct"),
                                                  column(12, dataTableOutput("corr_summary_enrichment"))
                                                  ),
                                                fluidRow(
                                                  column(12, plotlyOutput("enrichmentTable_single"))
                                                  )
                                                ),
                               conditionalPanel(condition = 'input.separate == "Yes"',
                                                fluidRow(
                                                  uiOutput("corr_summary_enrichment_instruct1"),
                                                  column(10, dataTableOutput("corr_summary_enrichment1"))
                                                  ),
                                                fluidRow(
                                                  column(12, plotlyOutput("enrichmentTable_split1"))
                                                  ),
                                                fluidRow(
                                                  uiOutput("corr_summary_enrichment_instruct2"),
                                                  column(10, dataTableOutput("corr_summary_enrichment2"))
                                                  ),
                                                fluidRow(
                                                  column(12, plotlyOutput("enrichmentTable_split2"))
                                                  )
                                                )
                               )
                             )
                           )
                         )
      )
      }
    })
  })


# Heatmap Action Button (correlations tab) --------------------------------
observeEvent(input$do_plot_btn, {
  if (input$separate == "No"){
    withBusyIndicatorServer("do_plot_btn", {
      h <- suppressWarnings(heatmap_cors(rows_selected = input$corr_summary_rows_selected,
                                         summary = data$summary,
                                         cormat = data$cormat))
      output$corr_plot <- renderPlotly({h})
    })
  } else{
    withBusyIndicatorServer("do_plot_btn", {
      # Condition 1
      browser()
      h1 <- suppressWarnings(heatmap_cors(rows_selected = input$corr_summary1_rows_selected,
                                          summary = data_split$summary1,
                                          cormat = data_split$cormat1))
      output$corr_plot1 <- renderPlotly({h1})
      
      # Condition 2
      h2 <- suppressWarnings(heatmap_cors(rows_selected = input$corr_summary2_rows_selected,
                                          summary = data_split$summary2,
                                          cormat = data_split$cormat2))
      output$corr_plot2 <- renderPlotly({h2})
    })
  }
})

# Making dynamic adjustments to heatmap
## Single heatmap
output$dynamic_corr_plot <- renderUI({
    height = paste(input$corr_plot_height, "px", sep="")
    width = paste(input$corr_plot_width, "px", sep="")
    plotlyOutput("corr_plot", width=width, height=height)
})

## Separated heatmaps
output$dynamic_corr_plot1 <- renderUI({
  height = paste(input$corr_plot_height, "px", sep="")
  width = paste(input$corr_plot_width, "px", sep="")
  plotlyOutput("corr_plot1", width=width, height=height)
})

output$dynamic_corr_plot2 <- renderUI({
  height = paste(input$corr_plot_height, "px", sep="")
  width = paste(input$corr_plot_width, "px", sep="")
  plotlyOutput("corr_plot2", width=width, height=height)
})

# Co-Occurence Network Action Button ----------------------------------------------------
observeEvent(input$do_network_btn, {
  # Together
  if (input$separate == "No"){
    grp <- data$summary[input$corr_summary_network_rows_selected, 1]
    if (input$assay2 == "MicrobeGenetics") {
      withBusyIndicatorServer("do_network_btn", {
        output$corrNetwork_single <- renderPlot({
          suppressWarnings(corr_network(MAE = vals$MAE,
                                        assay = input$assay2,
                                        cormat = data$cormat, 
                                        group = grp,
                                        tax_level = input$tax.level2,
                                        correction = input$p.adjust.network,
                                        alpha = input$alpha_network))
        })
      })
    } else {
      withBusyIndicatorServer("do_network_btn", {
        output$corrNetwork_single <- renderPlot({
          suppressWarnings(corr_network(MAE = vals$MAE,
                                        assay = input$assay2,
                                        cormat = data$cormat, 
                                        group = grp,
                                        correction = input$p.adjust.network,
                                        alpha = input$alpha_network))
        })
      })
    }
  
  # Separated by condition  
  } else {
    withBusyIndicatorServer("do_network_btn", {
      # Condition 1:
      grp1 <- data_split$summary1[input$corr_summary_network1_rows_selected, 1]
      grp2 <- data_split$summary2[input$corr_summary_network2_rows_selected, 1]
      if (input$assay2 == "MicrobeGenetics") {
        withBusyIndicatorServer("do_network_btn", {
          output$corrNetwork_split1 <- renderPlot({
            suppressWarnings(corr_network(MAE = vals$MAE,
                                          assay = input$assay2,
                                          cormat = data_split$cormat1, 
                                          group = grp1,
                                          tax_level = input$tax.level2,
                                          correction = input$p.adjust.network,
                                          alpha = input$alpha_network))
          })
          output$corrNetwork_split2 <- renderPlot({
            suppressWarnings(corr_network(MAE = vals$MAE,
                                          assay = input$assay2,
                                          cormat = data_split$cormat2, 
                                          group = grp2,
                                          tax_level = input$tax.level2,
                                          correction = input$p.adjust.network,
                                          alpha = input$alpha_network))
          })
        })
      } else {
        withBusyIndicatorServer("do_network_btn", {
          output$corrNetwork_split1 <- renderPlot({
            suppressWarnings(corr_network(MAE = vals$MAE,
                                          assay = input$assay2,
                                          cormat = data_split$cormat1, 
                                          group = grp1,
                                          correction = input$p.adjust.network,
                                          alpha = input$alpha_network))
          })
          output$corrNetwork_split2 <- renderPlot({
            suppressWarnings(corr_network(MAE = vals$MAE,
                                          assay = input$assay2,
                                          cormat = data_split$cormat2, 
                                          group = grp2,
                                          correction = input$p.adjust.network,
                                          alpha = input$alpha_network))
          })
        })
      }
      # First network
      # output$corrNetwork_split1 <- renderPlot({
      #   suppressWarnings(corr_network(MAE = vals$MAE,
      #                                 assay = input$assay2,
      #                                 cormat = data_split$cormat1, 
      #                                 group = input$networkOTU_split1))
      # })
      
      # Second network
      # output$corrNetwork_split2 <- renderPlot({
      #   suppressWarnings(corr_network(MAE = vals$MAE,
      #                                 assay = input$assay2,
      #                                 cormat = data_split$cormat2, 
      #                                 group = input$networkOTU_split2))
      # })
    })
  }
})


# Enrichment Action Button  -------------------------------------------------------------

observeEvent(input$do_enrich_btn, {
  if (input$separate == "No") {
    withBusyIndicatorServer("do_enrich_btn", {
      p_single <- enrich_cors(corr_results = data$summary, 
                              group_selected = data$summary[input$corr_summary_enrichment_rows_selected, 1], 
                              geneset_db = input$db)
      output$enrichmentTable_single <- renderPlotly({p_single})
    }) 
  } else if (input$separate == "Yes") {
    withBusyIndicatorServer("do_enrich_btn", {
      p_split1 <- enrich_cors(corr_results = data_split$summary1, 
                              group_selected = data_split$summary1[input$corr_summary_enrichment1_rows_selected, 1], 
                              geneset_db = input$db)
      output$enrichmentTable_split1 <- renderPlotly({p_split1})
      
      p_split2 <- enrich_cors(corr_results = data_split$summary2, 
                              group_selected = data_split$summary2[input$corr_summary_enrichment2_rows_selected, 1], 
                              geneset_db = input$db)
      output$enrichmentTable_split2 <- renderPlotly({p_split2})
    })
  }
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
