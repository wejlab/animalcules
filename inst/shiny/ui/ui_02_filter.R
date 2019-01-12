tabPanel("Summary and Filter",
  tabsetPanel(
    tabPanel("Summary",
      br(),
      sidebarLayout(
        sidebarPanel(
          br(),
          selectInput("filter_type", "Select Filter Type", c("By Metadata", "By Microbes")),

          # Discard samples
          selectizeInput("filter_sample_dis", "Discard Samples", choices=sam.name, multiple=TRUE),

          # Select conditons
          selectizeInput('filter_sample_cov', 'Select a Condition', choices=covariates, multiple=FALSE),

          # Filtering by sample
          conditionalPanel(condition = "input.filter_type == 'By Metadata'",
            uiOutput("filter_sample_cov_params")
          ),

          withBusyIndicatorUI(
            actionButton("filter_button", "Filter")
          ),
          withBusyIndicatorUI(
            actionButton("reset_button", "Reset")
          ),
          width=3
        ),
        mainPanel(
          fluidRow(
            column(6,
              br()
            ),
            column(6,
              plotlyOutput("filter_summary_top_plot", height="350px"),
              plotlyOutput("filter_summary_bottom_plot", height="350px")
            )
          ), 
          width=9
        )
      )
    )
  )
)