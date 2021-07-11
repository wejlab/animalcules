tabPanel("Correlation Analysis",
         tabsetPanel(id = "corr",
           tabPanel("Spearman's Correlation",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("assay1", "Select Assay 1:",
                                      choices = mae.assays),
                          selectInput("assay2", "Select Assay 2:",
                                      choices = mae.assays),
                          conditionalPanel(
                            condition = 'input.assay1 == "MicrobeGenetics"',
                            selectInput("tax.level1", "Select taxonomic level for Assay 1:",
                              choices = tax.name, selected = tax.default)
                          ),
                          conditionalPanel(
                            condition = 'input.assay2 == "MicrobeGenetics"',
                            selectInput("tax.level2", "Select taxonomic level for Assay 2:",
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
                                             # h2(textOutput("row_select_heatmap")),
                                             # h3(textOutput("row_select_instruct")),
                                             uiOutput("corr_summary_instruct"),
                                             column(12, dataTableOutput("corr_summary"))
                                             #,
                                             # checkboxInput("dt_sel", "Select / Deselect all rows"),
                                             # verbatimTextOutput("selected_rows", TRUE)
                                             ),
                                           fluidRow(
                                             column(12, uiOutput("dynamic_corr_plot"))
                                             )
                                           ),
                          conditionalPanel(condition = "input.separate == 'Yes'",
                                           # Summary tables
                                           fluidRow(
                                             uiOutput("corr_summary_split_instruct1"),
                                             column(6, dataTableOutput("corr_summary1")),
                                             # checkboxInput("dt_sel1", "Select / Deselect all"),
                                             # h4("Selected rows:"),
                                             # verbatimTextOutput("selected_rows1", TRUE),
                                             column(6, uiOutput("dynamic_corr_plot1")),
                                             # checkboxInput("dt_sel2", "Select / Deselect all"),
                                             # h4("Selected rows:"),
                                             # verbatimTextOutput("selected_rows2", TRUE)
                                             ),
                                           
                                           # Heat maps
                                           fluidRow(
                                             uiOutput("corr_summary_split_instruct2"),
                                             column(6, dataTableOutput("corr_summary2")),
                                             column(6, uiOutput("dynamic_corr_plot2"))
                                             )
                                           )
                          )
                        )
                      )
                    )
           )
         )