
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
                                                                    options = list(scrollX = T),
                                                                    selection = "single"
                                                                    )}, server = FALSE)
      output$corr_summary_network_instruct <- renderUI({
        HTML("<h2>Select row to create network</h2>")
      })
      
      
      # Summary table --> Enrichment tab
      output$corr_summary_enrichment <- renderDataTable({DT::datatable(result$summary,
                                                                       options = list(scrollX = T),
                                                                       selection = "single"
                                                                       )}, server = FALSE)
      output$corr_summary_enrichment_instruct <- renderUI({
        HTML("<h2>Select a row to calculate enrichment for:</h2>")
      })

      # Storing results for later
      data$summary <- result$summary
      data$cormat <- result$cormat
      
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
        output$corr_summary_split_instruct1 <- renderUI({
          HTML("
        <h2>Select rows to plot on heat map</h2>
        <p>Shift + Click to select contiguous rows, 
        Ctrl/Command + Click to select separate rows</p>
             ")
        })
        
        output$corr_summary_split_instruct2 <- renderUI({
          HTML("
        <p>Shift + Click to select contiguous rows, 
        Ctrl/Command + Click to select separate rows</p>
             ")
        })
        

        # For Co-Occurence Networks
        
        ## Summary Tables
        output$corr_summary_network1 <- renderDataTable({DT::datatable(result_con1$summary,
                                                                       caption = c1,
                                                                       options = list(scrollX = T),
                                                                       selection = "single"
                                                                       )}, server = FALSE)
      
        output$corr_summary_network2 <- renderDataTable({DT::datatable(result_con2$summary,
                                                                       caption = c2,
                                                                       options = list(scrollX = T),
                                                                       selection = "single"
                                                                       )}, server = FALSE)
        
        ## Instructions
        output$corr_summary_network_split_instruct_ovr <- renderUI({
          HTML("<h2>Select one row from each table to create network</h2>")
        })

        output$corr_summary_network_split_instruct1 <- renderUI({
          HTML("<h3>Select first row: </h3>")
        })
        
        output$corr_summary_network_split_instruct2 <- renderUI({
          HTML("<h3>Select second row: </h3>")
        })
        
        
        # For Enrichment
        
        ## Summary Tables
        output$corr_summary_enrichment1 <- renderDataTable({DT::datatable(result_con1$summary,
                                                                          caption = c1,
                                                                          options = list(scrollX = T),
                                                                          selection = "single"
                                                                          )}, server = FALSE)
        
        output$corr_summary_enrichment2 <- renderDataTable({DT::datatable(result_con2$summary,
                                                                          caption = c2,
                                                                          options = list(scrollX = T),
                                                                          selection = "single"
                                                                          )}, server = FALSE)
        ## Instructions
        output$corr_summary_enrichment_instruct_ovr <- renderUI({
          HTML("<h2>Select one row from each table to calculate enrichment</h2>")
        })
        
        output$corr_summary_enrichment_instruct1 <- renderUI({
          HTML("<h3>Select first row:</h3>")
        })
        output$corr_summary_enrichment_instruct2 <- renderUI({
          HTML("<h3>Select second row:</h3>")
          })
        
        
        # Storing results for later
        data_split$summary1 <- result_con1$summary
        data_split$cormat1 <- result_con1$cormat
        data_split$summary2 <- result_con2$summary
        data_split$cormat2 <- result_con2$cormat
        
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
                                                  uiOutput("corr_summary_network_split_instruct_ovr"),
                                                  uiOutput("corr_summary_network_split_instruct1"),
                                                  column(6, dataTableOutput("corr_summary_network1")),
                                                  column(6,plotOutput("corrNetwork_split1"))
                                                  ),
                                                fluidRow(
                                                  uiOutput("corr_summary_network_split_instruct2"),
                                                  column(6, dataTableOutput("corr_summary_network2")),
                                                  column(6,plotOutput("corrNetwork_split2"))
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
                                                  uiOutput("corr_summary_enrichment_instruct_ovr"),
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
                               #                  fluidRow(
                               #                    uiOutput("corr_summary_enrichment_instruct1"),
                               #                    column(4, dataTableOutput("corr_summary_enrichment1")),
                               #                    column(8, plotlyOutput("enrichmentTable_split1"))
                               #                    ),
                               #                  fluidRow(
                               #                    uiOutput("corr_summary_enrichment_instruct2"),
                               #                    column(4, dataTableOutput("corr_summary_enrichment2")),
                               #                    column(8, plotlyOutput("enrichmentTable_split2"))
                               #                    ),
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
      #browser()
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