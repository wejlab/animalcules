tabPanel("Correlation Analysis",
         tabsetPanel(
           tabPanel("Correlation Matrix",
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
                          conditionalPanel(
                            condition = 'input.assay2 == "hostExpression" | input.assay2 == "hervAbundance"',
                            sliderInput('no.sig', "Select correlation group size \n(min is 1, max is 100):",
                                        value = 50, min = 1, max = 150, step = 10)
                            ),
                          checkboxInput("dispOpt", "Show Plot Display Options",
                                        FALSE
                          ),
                          conditionalPanel(
                            condition = 'input.dispOpt == true',
                            radioButtons("axis_lab", "Choose axis labels to hide:",
                                         choices = c("None selected" = "na",
                                                     "Hide y axis labels" = "yax",
                                                     "Hide x axis labels" = "xax",
                                                     "Hide both" = "bax"))
                            ),
                          # Button to run the correlation
                          withBusyIndicatorUI(
                            actionButton("runCorr", "Run Correlation Analysis")
                          ),

                        ),
                        mainPanel(
                          # Display the results
                          tabsetPanel(
                            tabPanel("Data Summary", dataTableOutput("corr_summary")),
                            tabPanel("Heat map", plotlyOutput("corr_plot",
                                                              height = "800px"))
                          )
                        )
                      )
                    )
           ),
           tabPanel("Enrichment Analysis",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("mList"),
                          selectInput("db", "Select database for enrichR:",
                                      dbList),
                          withBusyIndicatorUI(
                            actionButton("enrich", "Run enrichment analysis")
                            ),
                        ),
                        mainPanel(
                          dataTableOutput("enrichmentTable")
                        )
                      )
                    )
           )
         ))
