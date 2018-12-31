

tabPanel("Differential Analysis",
    tabsetPanel(
         tabPanel("Differential Abundance",
                  selectInput("DAmethod", "Select method",
                    c("DESeq2")),
                  conditionalPanel(condition = "input.DAmethod == 'DESeq2'",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectizeInput('taxl.da', 'Taxonomy Level', choices = tax.name,
                                                      selected='no rank'),
                                       selectizeInput('da_condition', 'Select condition',
                                                      choices = covariates),
                                       conditionalPanel(condition = "output.da_condition_type == 'multiple'",
                                                        helpText("Please select 2 levels to compare"),
                                                        uiOutput("da_condition_options")
                                       ),
                                       selectizeInput('da.condition.covariate', 'Select (multiple) covariates',
                                                      choices = covariates, multiple = TRUE),
                                       helpText("Continuous covariates would be automatically cut into factors with 3 levels."),
                                       numericInput('da.count.cutoff', 'Minumum count cut-off', 500,
                                                    min = 1, max = 5000),
                                       numericInput('da.padj.cutoff', 'Choose padj cut-off', 0.5,
                                                    min = 1e-100, max = 1),
                                       actionButton("run_deseq2", "Run"),
                                       width=3
                                     ),
                                     mainPanel(
                                       tabPanel("DeSeq2",
                                         DT::dataTableOutput("DeSeq2Table.new")
                                       ), width=9
                                     )
                                   )
                                   )
         )
    )


)
