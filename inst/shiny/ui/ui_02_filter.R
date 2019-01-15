tabPanel("Summary and Filter",
  tabsetPanel(
    tabPanel("Summary",
      br(),
      sidebarLayout(
        sidebarPanel(
          # View Style
          selectInput("filter_type", "Filter By", c("Metadata", "Microbes")),

          # Metadata
          conditionalPanel(condition = "input.filter_type == 'Metadata'",
            selectizeInput('filter_type_metadata', 'Select a Condition', choices=covariates, multiple=FALSE),
            uiOutput("filter_metadata_params"),
            withBusyIndicatorUI(
              actionButton("filter_metadata_btn", "Filter")
            )
          ),

          # Microbes
          conditionalPanel(condition = "input.filter_type == 'Microbes'",
            selectInput("filter_type_microbes", "Select Filter Condition", c("Mapped Read Number",
                                                                             "Relative Abundace",
                                                                             "Prevalence")
            )
          ),
          conditionalPanel(condition = "input.filter_type == 'Microbes' & input.filter_type_microbes == 'Mapped Read Number'",
            numericInput("filter_microbes_read_inp", "Average Minimum Reads", 0, min = 0, max = 10000),
            withBusyIndicatorUI(
              actionButton("filter_microbes_read_btn", "Filter")
            )
          ),
          conditionalPanel(condition = "input.filter_type == 'Microbes' & input.filter_type_microbes == 'Relative Abundace'",
            sliderInput("filter_microbes_rela_inp", "Restrict To", min = 0, max = 1, value = c(0,1)),
            withBusyIndicatorUI(
              actionButton("filter_microbes_rela_btn", "Filter")
            )
          ),
          conditionalPanel(condition = "input.filter_type == 'Microbes' & input.filter_type_microbes == 'Prevalence'",
            sliderInput("filter_microbes_prev_inp", "Restrict To", min = 0, max = 1, value = c(0,1)),
            withBusyIndicatorUI(
              actionButton("filter_microbes_prev_btn", "Filter")
            )
          ),

          br(),

          # Discard samples
          selectizeInput("filter_sample_dis", "Discard Samples", choices=sam.name, multiple=TRUE),
          withBusyIndicatorUI(
            actionButton("filter_sample_dis_btn", "Discard")
          ),

          br(),
          # Reset
          withBusyIndicatorUI(
            actionButton("filter_reset_btn", "Reset")
          ),
          width=3
        ),
        mainPanel(
          fluidRow(
            column(6,
              plotlyOutput("filter_summary_top_plot", height="350px"),
              plotlyOutput("filter_summary_bottom_plot", height="350px")
            ),
            column(6,
              br()
            )
          ), 
          width=9
        )
      )
    ),
    tabPanel("Categorize",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          selectizeInput('filter_bin_cov', 'Covariate', choices=num_covariates, multiple=FALSE),
          uiOutput("filter_nbins"),
          textInput('filter_bin_breaks', 'Custom Breaks (Comma Delimited)'),
          verbatimTextOutput("filter_bin_to1"),
          textInput('filter_bin_labels', 'Custom Labels (Comma Delimited)'),
          verbatimTextOutput("filter_bin_to2"),
          textInput("filter_new_covariate", "Covariate Label", value = "new_cov"),
          actionButton("filter_create_bins", "Create Bins"),
          width=5
        ),
        mainPanel(
          plotlyOutput("filter_unbin_plot", height="200px"),
          br(),
          plotlyOutput("filter_bin_plot"),
          width=7
        )
      )
    )
  )
)