tabPanel("Biomarker",
  br(),
  sidebarLayout(
    sidebarPanel(
      br(),
      selectizeInput('taxl_biomarker', 'Taxonomy Level', choices = tax.name,
                     selected='genus'),
      selectInput("select_target_condition_biomarker", "Select Target Condition:",
                  covariates.colorbar),
      conditionalPanel(condition = "output.biomarker_condition_type == 'multiple'",
                       helpText("Please select 2 levels to compare"),
                       uiOutput("biomarker_condition_options")
      ),
      checkboxInput("biomarker_adv", "Advanced Options"),
      conditionalPanel(
        condition = "input.biomarker_adv == true",
        numericInput("num.cv.nfolds", "Number of CV nfolds", value = 3, max = 20, min = 3)
      ),
      conditionalPanel(
        condition = "input.biomarker_adv == true",
        numericInput("num.biomarker.run", "Number of CV repeats", value = 3, max = 100, min = 3)
      ),
      conditionalPanel(
        condition = "input.biomarker_adv == true",
        numericInput("percent_top_biomarker", "Top biomarker proportion", value = 0.2, max = 1, min = 0.01)
      ),
      conditionalPanel(
        condition = "input.biomarker_adv == true",
        selectInput("select_model_biomarker", "Select Model", c("logistic regression", "random forest"))
      ),

      withBusyIndicatorUI(
        actionButton("goButtonBiomarker",
                     "Run",
                     class = "btn-primary")
      ),
      width=3
   ),
   mainPanel(
    tabsetPanel(
      tabPanel("Biomarker",
               br(),
               tableOutput("biomarker_list")
        ),
        tabPanel("Importance plot",
                 br(),
                 plotOutput("importance_plot",height=300)
        ),
        tabPanel("CV ROC plot",
                 br(),
                 plotOutput("roc_plot")
        )
      ), width=9
    )
  )
)
