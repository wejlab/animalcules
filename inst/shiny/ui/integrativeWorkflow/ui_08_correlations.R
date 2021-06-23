tabPanel("Correlation Analysis",
         tabsetPanel(id = "corr",
           tabPanel("Correlating Assays",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("assay1", "Select first assay:",
                                      choices = mae.assays),
                          selectInput("assay2", "Select second assay:",
                                      choices = mae.assays),
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
                          radioButtons(
                            "separate", "Separate by condition?",
                            choices = c("Yes", "No"), selected = "No"
                          ),
                          conditionalPanel(
                            condition = 'input.separate == "Yes"',
                            selectInput("select_correlation_condition", "Select condition:", 
                                        choices = covariates.two.levels)
                          ),
                          
                          checkboxInput("advOptions", "Advanced Correlation Parameters",
                                        FALSE),
                          
                          conditionalPanel(
                            condition = 'input.advOptions == true',
                            numericInput('no.sig', "Select minimum correlation group size:",
                              value = 1, min = 1, max = 600, step = 1),
                            selectInput("correction", "Select p-value correction method",
                              choices = stats::p.adjust.methods, selected = stats::p.adjust.methods[4]),
                            numericInput("alpha", "Select significance threshold",
                              value = 0.05, min = 0.001, max = 0.1, step = 0.001)
                          ),
                          
                          checkboxInput("adv_plot_options", "Plot Display Options", 
                                        value = FALSE),
                          
                          conditionalPanel(condition = "input.adv_plot_options == true",
                            sliderInput("corr_plot_height", "Plot Height",
                              400, 1200, value = 600, step = 50, post = "px"),
                            sliderInput("corr_plot_width", "Plot Width",
                              400, 1200, value = 800, step = 50, post = "px")
                          ),
                          
                          withBusyIndicatorUI(
                            actionButton("do_corr_btn", "Run correlation analysis", class = "btn-primary")
                            ),
                          withBusyIndicatorUI(
                            actionButton("do_plot_btn", "Plot heatmap")
                            )
                        ),
                        mainPanel(
                          conditionalPanel(condition = "input.separate == 'No'",
                                           fluidRow(
                                             column(12, dataTableOutput("corr_summary")),
                                             checkboxInput("dt_sel", "Select / Deselect all"),
                                             h4("Selected rows:"),
                                             verbatimTextOutput("selected_rows", TRUE)
                                             ),
                                           fluidRow(
                                             column(12, uiOutput("dynamic_corr_plot"))
                                             )
                                           ),
                          conditionalPanel(condition = "input.separate == 'Yes'",
                                           fluidRow(
                                             column(5, dataTableOutput("corr_summary1")),
                                             column(7, uiOutput("dynamic_corr_plot1"))
                                             ),
                                           fluidRow(
                                             column(5, dataTableOutput("corr_summary2")),
                                             column(7, uiOutput("dynamic_corr_plot2"))
                                             )
                                           )
                          )
                        )
                      )
                    )#,
           # tabPanel("Enrichment",
           #          fluidPage(
           #            sidebarLayout(
           #              sidebarPanel(
           #                conditionalPanel(condition = 'input.separate == "No"',
           #                                 uiOutput("gList_enrich_single")
           #                                 ),
           #                conditionalPanel(condition = 'input.separate == "Yes"',
           #                                 uiOutput("gList_enrich_split1"),
           #                                 uiOutput("gList_enrich_split2")
           #                                 ),
           #                selectInput("db", "Select database for enrichR:",
           #                            dbList
           #                            ),
           #                withBusyIndicatorUI(
           #                  actionButton("do_enrich_btn", "Calculate Enrichment", class = "btn-primary")
           #                ),
           #              ),
           #              mainPanel(
           #                conditionalPanel(condition = 'input.separate == "No"',
           #                                 fluidRow(
           #                                   column(12, plotlyOutput("enrichmentTable_single"))
           #                                   )
           #                                 ),
           #                conditionalPanel(condition = 'input.separate == "Yes"',
           #                                 fluidRow(column(12, plotlyOutput("enrichmentTable_split1"))),
           #                                 fluidRow(column(12, plotlyOutput("enrichmentTable_split2")))
           #                                 )
           #                )
           #              )
           #            )
                    #),
           # tabPanel("Co-Occurence Networks",
           #          fluidPage(
           #            sidebarLayout(
           #              sidebarPanel(
           #                conditionalPanel(condition = "input.separate == 'No'",
           #                                 uiOutput("gList_network_single")
           #                                 ),
           #                conditionalPanel(condition = "input.separate == 'Yes'",
           #                                 uiOutput("gList_split1"),
           #                                 uiOutput("gList_split2")
           #                                 ),
           #                withBusyIndicatorUI(actionButton("do_network_btn", "Plot Network"))
           #              ),
           #              mainPanel(
           #                conditionalPanel(condition = "input.separate == 'No'",
           #                                 fluidRow(
           #                                   column(12, 
           #                                          plotOutput("corrNetwork_single"))
           #                                   )
           #                                 ),
           #                conditionalPanel(condition = "input.separate == 'Yes'",
           #                                 fluidRow(
           #                                   column(6,
           #                                          plotOutput("corrNetwork_split1")),
           #                                   column(6,
           #                                          plotOutput("corrNetwork_split2"))
           #                                   )
           #                                 )
           #                )
           #              )
           #            )
           #          )
           )
         )