tabPanel("Differential Analysis",
  sidebarLayout(
    sidebarPanel(
      selectizeInput('taxl.da', 'Taxonomy Level', choices = tax.name,
                     selected=tax.default),
      selectizeInput('da_condition', 'Select condition',
                     choices = covariates.colorbar),
      conditionalPanel(condition = "output.da_condition_type == 'multiple'",
                       helpText("Please select 2 levels to compare"),
                       uiOutput("da_condition_options")
      ),
      checkboxInput("da_adv", "Advanced Options"),
      conditionalPanel(
        condition = "input.da_adv == true",
        selectizeInput('da.condition.covariate', 'Select (multiple) covariates',
        choices = covariates, multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.da_adv == true",
        numericInput('da.count.cutoff', 'Minumum count cut-off', 500,
                   min = 1, max = 5000)
      ),
      conditionalPanel(
        condition = "input.da_adv == true",
        numericInput('da.padj.cutoff', 'Choose padj cut-off', 0.5,
                    min = 1e-100, max = 1)
      ),
      #helpText("Continuous covariates would be automatically cut into factors with 3 levels."),

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
