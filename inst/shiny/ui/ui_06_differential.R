tabPanel("Differential Analysis",
  sidebarLayout(
    sidebarPanel(
      selectizeInput('taxl.da', 'Taxonomy Level', choices = tax.name,
                     selected=tax.default),
      selectizeInput('da_condition', 'Select condition',
                     choices = covariates),
      checkboxInput("da_adv", "Advanced Options"),
      conditionalPanel(
        condition = "input.da_adv == true",
        selectizeInput('da_condition_covariate', 'Select (multiple) covariates',
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

      actionButton("run_deseq2", "Run"),
      width=3
    ),
    mainPanel(
      tabPanel("DeSeq2",
        helpText("Note: For multi-level target viariable, all significant results will be printed if existed"),
        DT::dataTableOutput("DeSeq2Table.new")
      ), width=9
    )
  )
)
