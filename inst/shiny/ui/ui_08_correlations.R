tabPanel("Correlation Analysis",
         tabsetPanel(
           tabPanel("Correlating Assays",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          # Selecting which assays to run
                          selectInput("assay1", "Select first assay:",
                                       choices = mae.assays
                          ),
                          selectInput("assay2", "Select second assay:",
                                       choices = mae.assays
                          ),
                          conditionalPanel(
                            condition = 'input.assay1 == "MicrobeGenetics"',
                            selectInput("tax.level1", "Select taxonomic level for assay 1:",
                                         choices = tax.name, selected = tax.default)
                            ),
                          conditionalPanel(
                            condition = 'input.assay2 == "MicrobeGenetics"',
                            selectInput("tax.level2", "Select taxonomic level for assay 2:",
                                         choices = tax.name, selected = tax.default)
                            ),
                          checkboxInput("advOptions", "Show Advanced Options",
                                        FALSE
                          ),
                          conditionalPanel(
                            condition = 'input.advOptions == true',
                            #conditionalPanel(
                              #condition = 'input.assay2 == "hostExpression" | input.assay2 == "hervAbundance"',
                              sliderInput('no.sig', "Select correlation group size \n(min is 1, max is 100):",
                                          value = 50, min = 1, max = 150, step = 10),
                            #),
                            selectInput("correction", "Select p-value correction method",
                                        choices = stats::p.adjust.methods,
                                        selected =stats::p.adjust.methods[4]),
                            numericInput("alpha", "Select significance threshold",
                                        value = 0.05, min = 0.001, max = 0.1, step=0.001)
                            ),
                            # radioButtons("axis_lab", "Choose axis labels to hide:",
                            #              choices = c("None selected" = "na",
                            #                          "Hide y axis labels" = "yax",
                            #                          "Hide x axis labels" = "xax",
                            #                          "Hide both" = "bax"))
                          # Button to run the correlation
                          withBusyIndicatorUI(
                            actionButton("do_corr_btn", "Run correlation analysis", class = "btn-primary"))
                          ),
                    mainPanel(
                      # Display the results
                      fluidRow(
                        column(12,
                               dataTableOutput("corr_summary"))
                        )
                          # tabsetPanel(
                          #   tabPanel("Data Summary", dataTableOutput("corr_summary")),
                          #   tabPanel("Heat map", plotlyOutput("corr_plot",
                          #                                     height = "800px"))
                          # )
                        )
                    )
                    )
                    ),
           tabPanel("Heatmap",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   withBusyIndicatorUI(
                     actionButton("do_plot_btn", "Plot heatmap")
                     ),
                   checkboxInput("adv_plot_options", "Advanced plot options", value = FALSE),
                   conditionalPanel(condition = "input.adv_plot_options == true",
                                    sliderInput("corr_plot_height", "Plot Height", 400, 1200, value=600, step=50, post="px"),
                                    sliderInput("corr_plot_width", "Plot Width", 400, 1200, value=800, step=50, post="px")
                   )
                                    
                 ),
                 mainPanel(
#                   fluidRow(
#                     column(12,
                            #plotlyOutput("corr_plot")
                   uiOutput("dynamic_corr_plot"),
                   width=9
#                     )
#                   )
                 )
               )
             )
           ),
           tabPanel("Enrichment Analysis",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("gList"),
                          selectInput("db", "Select database for enrichR:",
                                      dbList),
                          withBusyIndicatorUI(
                            actionButton("do_enrich_btn", "Calculate Enrichment", class = "btn-primary")
                            ), 
                          ),
                        mainPanel(
                          plotlyOutput("enrichmentTable")
                          )
                        )
                      )
                    )
           )
         )
                    
                    # fluidPage(
                    #   sidebarLayout(
                    #     sidebarPanel(
                    #       uiOutput("mList"),
                    #       selectInput("db", "Select database for enrichR:",
                    #                   dbList),
                    #       withBusyIndicatorUI(
                    #         actionButton("enrich", "Run enrichment analysis")
                    #         ),
                    #     ),
                        # mainPanel(
                        #   dataTableOutput("enrichmentTable")
                        # )
                    #   )
                    # ))