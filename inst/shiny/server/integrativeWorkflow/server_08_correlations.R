
# Placeholders
data <- reactiveValues(summary_table = NULL,
                       cormat = NULL)
data_split <- reactiveValues(summary_table1 = NULL,
                             cormat1 = NULL,
                             summary_table2 = NULL,
                             cormat2 = NULL)
r_select <- reactiveVal(NULL)

observeEvent(input$do_corr_btn, {
  withBusyIndicatorServer("do_corr_btn", {
    
    # Removing previous tabs
    removeTab(inputId = "corr",
              target = "Co-Occurence Networks")
    removeTab(inputId = "corr",
              target = "Enrichment")
    
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
    
    # Perform correlation: separate
    
    if(input$separate == "No"){
      result <- suppressWarnings(corr_func(MAE = vals$MAE,
                                           asys = as,
                                           tax_level = t,
                                           no.sig = n,
                                           correction = c,
                                           alpha = al)) 
      # Summary table
      output$corr_summary <- renderDataTable({DT::datatable(result$summary,
                                                            extensions = "Buttons",
                                                            options = list(
                                                              scrollX = T, 
                                                              buttons = c("copy", "csv"))
                                                            )}, server = TRUE)
      output$corr_summary_network <- renderDataTable({DT::datatable(result$summary,
                                                                    extensions = "Buttons",
                                                                    options = list(
                                                                      scrollX = T, 
                                                                      buttons = c("copy", "csv"))
                                                                    )}, server = TRUE)
      r_select <- input$corr_summary_rows_selected
      
      # group list for enrichment analysis
      output$gList_enrich_single <- renderUI({ 
        selectInput("enrichOTU_single", "Select Group for enrichR analysis:",
                    result$summary$OTU)
        })
      
      # group list for network analysis
      output$gList_network_single <- renderUI({
        selectInput("networkOTU_single", "Create co-occurence network for which Group?",
                    result$summary$OTU)
        })
      
      # Storing results for later
      data$summary_table <- result$summary
      data$cormat <- result$cormat
      
      } else if (input$separate == "Yes") { # Split into two conditions
        # %<-% comes from `zeallot` package
        c(con1, con2) %<-% separate_by_condition(vals$MAE, input$select_correlation_condition)
      
        # Condition 1: Results + summary table
        result_con1 <- suppressWarnings(corr_func(MAE = con1,
                                                  asys = as,
                                                  tax_level = t,
                                                  no.sig = n,
                                                  correction = c,
                                                  alpha = al))
        c1 <- paste0('Summary for condition: "', colData(con1)[input$select_correlation_condition][1,1], '"', 
                     collapse = "")
        output$corr_summary1 <- renderDataTable({DT::datatable(result_con1$summary,
                                                               options = list(
                                                                 scrollX = T,
                                                                 buttons = c("copy", "csv")),
                                                               caption = c1
                                                               )})
        
        
        # Condition 2: Results + summary table
        result_con2 <- suppressWarnings(corr_func(MAE = con2,
                                                  asys = as,
                                                  tax_level = t,
                                                  no.sig = n,
                                                  correction = c,
                                                  alpha = al))
        c2 <- paste0('Summary for condition: "', colData(con2)[input$select_correlation_condition][1,1], '"', 
                     collapse = "")
        output$corr_summary2 <- renderDataTable({DT::datatable(result_con2$summary,
                                                               options = list(
                                                                 scrollX = T,
                                                                 buttons = c("copy", "csv")),
                                                               caption = c2
                                                               )})
        
        # Enrichment lists
        output$gList_enrich_split1 <- renderUI({
          selectInput("enrichOTU_split1", "Select Group 1 for enrichR analysis:",
                      result_con1$summary$OTU)
          })
        output$gList_enrich_split2 <- renderUI({
          selectInput("enrichOTU_split2", "Select Group 2 for enrichR analysis:",
                      result_con2$summary$OTU)
          })
      
      # Network lists
      output$gList_split1 <- renderUI({
        selectInput("networkOTU_split1", "Select Group 1 for network analysis:",
                    result_con1$summary$OTU)
        })
      
      output$gList_split2 <- renderUI({
        selectInput("networkOTU_split2", "Select Group 2 for network analysis:",
                    result_con2$summary$OTU)
        })
      
      # Storing results for later
      data_split$summary_table1 <- result_con1$summary
      data_split$cormat1 <- result_con1$cormat
      data_split$summary_table2 <- result_con2$summary
      data_split$cormat2 <- result_con2$cormat
      }

    # Dynamically displaying co-occurence network tab
    if(exists("result") | exists("result_con1")) {
      appendTab(inputId = "corr",
                tabPanel("Co-Occurence Networks",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               conditionalPanel(condition = "input.separate == 'No'",
                                                #uiOutput("gList_network_single")
                               ),
                               conditionalPanel(condition = "input.separate == 'Yes'",
                                                uiOutput("gList_split1"),
                                                uiOutput("gList_split2")
                               ),
                               withBusyIndicatorUI(actionButton("do_network_btn", "Plot Network"))
                             ),
                             mainPanel(
                               conditionalPanel(condition = "input.separate == 'No'",
                                                fluidRow(
                                                  column(12, dataTableOutput("corr_summary_network"))
                                                ),
                                                fluidRow(
                                                  column(12, 
                                                         plotOutput("corrNetwork_single"))
                                                )
                               ),
                               conditionalPanel(condition = "input.separate == 'Yes'",
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
    
    # Dynamically displaying Enrichment tab
    if(input$assay2 == "hostExpression") {
      appendTab(inputId = "corr",
                tabPanel("Enrichment",
                         fluidPage(
                           sidebarLayout(
                             sidebarPanel(
                               conditionalPanel(condition = 'input.separate == "No"',
                                                uiOutput("gList_enrich_single")
                               ),
                               conditionalPanel(condition = 'input.separate == "Yes"',
                                                uiOutput("gList_enrich_split1"),
                                                uiOutput("gList_enrich_split2")
                               ),
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
                                                  column(12, plotlyOutput("enrichmentTable_single"))
                                                )
                               ),
                               conditionalPanel(condition = 'input.separate == "Yes"',
                                                fluidRow(column(12, plotlyOutput("enrichmentTable_split1"))),
                                                fluidRow(column(12, plotlyOutput("enrichmentTable_split2")))
                               )
                             )
                           )
                         )
                )
                )
    }
    })
})

# Plotting heatmap
observeEvent(input$do_plot_btn, {
  if (input$separate == "No"){
    withBusyIndicatorServer("do_plot_btn", {
      sub_cormat <- subset_cormat_by_row(input$corr_summary_rows_selected,
                                         data$summary_table,
                                         data$cormat)
      h <- heatmap_cors(sub_cormat, hide_ax=NA)
      output$corr_plot <- renderPlotly({h})
    })
  } else{
    withBusyIndicatorServer("do_plot_btn", {
      # Condition 1
      h1 <- heatmap_cors(data_split$cormat1, hide_ax=NA)
      output$corr_plot1 <- renderPlotly({h1})
      # Condition 2
      h2 <- heatmap_cors(data_split$cormat2, hide_ax=NA)
      output$corr_plot2 <- renderPlotly({h2})
    })
  }
})

# Making dynamic adjustments to heatmap
output$dynamic_corr_plot <- renderUI({
    height = paste(input$corr_plot_height, "px", sep="")
    width = paste(input$corr_plot_width, "px", sep="")
    plotlyOutput("corr_plot", width=width, height=height)
})

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

# Enrichment Analysis
observeEvent(input$do_enrich_btn, {
  if (input$separate == "No") {
    withBusyIndicatorServer("do_enrich_btn", {
      p_single <- enrich_cors(corr_results = data$summary_table, 
                              group_selected = input$enrichOTU_single, 
                              geneset_db = input$db)
      output$enrichmentTable_single <- renderPlotly({p_single})
    }) 
  } else if (input$separate == "Yes") {
    withBusyIndicatorServer("do_enrich_btn", {
      p_split1 <- enrich_cors(corr_results = data_split$summary_table1, 
                              group_selected = input$enrichOTU_split1, 
                              geneset_db = input$db)
      output$enrichmentTable_split1 <- renderPlotly({p_split1})
      
      p_split2 <- enrich_cors(corr_results = data_split$summary_table2, 
                              group_selected = input$enrichOTU_split2, 
                              geneset_db = input$db)
      output$enrichmentTable_split2 <- renderPlotly({p_split2})
    })
  }
})

# Network 
observeEvent(input$do_network_btn, {
  if (input$separate == "No"){
    r_select <- input$corr_summary_network_rows_selected
    grp <- data$summary_table[r_select, 1]
    if (input$assay2 == "MicrobeGenetics") {
      withBusyIndicatorServer("do_network_btn", {
        output$corrNetwork_single <- renderPlot({
          suppressWarnings(corr_network(MAE = vals$MAE,
                                        assay = input$assay2,
                                        cormat = data$cormat, 
                                        group = grp,
                                        tax_level = input$tax.level2))
        })
      })
    } else {
      withBusyIndicatorServer("do_network_btn", {
        output$corrNetwork_single <- renderPlot({
          suppressWarnings(corr_network(MAE = vals$MAE,
                                        assay = input$assay2,
                                        cormat = data$cormat, 
                                        group = grp))
        })
      })
    }
    
  } else {
    withBusyIndicatorServer("do_network_btn", {
      # First network
      output$corrNetwork_split1 <- renderPlot({
        suppressWarnings(corr_network(MAE = vals$MAE,
                                      assay = input$assay2,
                                      cormat = data_split$cormat1, 
                                      group = input$networkOTU_split1))
      })
      
      # Second network
      output$corrNetwork_split2 <- renderPlot({
        suppressWarnings(corr_network(MAE = vals$MAE,
                                      assay = input$assay2,
                                      cormat = data_split$cormat2, 
                                      group = input$networkOTU_split2))
      })
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
