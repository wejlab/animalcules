tabPanel("Differential Analysis",
  tabsetPanel(
    tabPanel("DESeq2",
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
        numericInput('da.padj.cutoff', 'Choose padj cut-off', 0.8,
                    min = 1e-100, max = 1)
      ),
      withBusyIndicatorUI(
      actionButton("run_deseq2", "Run", class = "btn-primary")
      ),
      width=3
    ),
    mainPanel(
      tabPanel("DeSeq2",
        helpText("Note: For multi-level target viariable, all significant results will be printed if existed"),
        DT::dataTableOutput("DeSeq2Table.new")
      ), width=9
    )
  )
  ),
    tabPanel("limma",
  sidebarLayout(
    sidebarPanel(
      selectizeInput('taxl.da_limma', 'Taxonomy Level', choices = tax.name,
                     selected=tax.default),
      selectizeInput('da_condition_limma', 'Select condition',
                     choices = covariates),
      checkboxInput("da_adv_limma", "Advanced Options"),
      conditionalPanel(
        condition = "input.da_adv_limma == true",
        selectizeInput('da_condition_covariate_limma', 'Select (multiple) covariates',
        choices = covariates, multiple = TRUE)
      ),

      conditionalPanel(
        condition = "input.da_adv_limma == true",
        helpText("Note: Limma uses the logcpm assay, will automatically take log10 to the cut-off"),
        numericInput('da.count.cutoff_limma', 'Minumum count cut-off', 500,
                   min = 1, max = 5000)
      ),
      conditionalPanel(
        condition = "input.da_adv_limma == true",
        numericInput('da.padj.cutoff_limma', 'Choose padj cut-off', 0.8,
                    min = 1e-100, max = 1)
      ),
      withBusyIndicatorUI(
      actionButton("run_limma", "Run", class = "btn-primary")
      ),
      width=3
    ),
    mainPanel(
      tabPanel("limma",
        DT::dataTableOutput("limmaTable.new")
      ), width=9
    )
  )
  )
  )
)
