tabPanel("Summary and Filter",
  tabsetPanel(
    tabPanel("Filter",
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
            selectInput("filter_type_microbes", "Select Filter Condition", c("Average Read Number",
                                                                             "Average Relative Abundance",
                                                                             "Average Prevalence")
            )
          ),
          conditionalPanel(condition = "input.filter_type == 'Microbes' & input.filter_type_microbes == 'Average Read Number'",
            numericInput("filter_microbes_read_inp", "Set Minimum", 0, min = 0, max = 10000),
            withBusyIndicatorUI(
              actionButton("filter_microbes_read_btn", "Filter")
            )
          ),
          conditionalPanel(condition = "input.filter_type == 'Microbes' & input.filter_type_microbes == 'Average Relative Abundance'",
            sliderInput("filter_microbes_rela_inp", "Restrict To", min = 0, max = 1, value = c(0,1), step=0.0001),
            withBusyIndicatorUI(
              actionButton("filter_microbes_rela_btn", "Filter")
            )
          ),
          conditionalPanel(condition = "input.filter_type == 'Microbes' & input.filter_type_microbes == 'Average Prevalence'",
            sliderInput("filter_microbes_prev_inp", "Restrict To", min = 0, max = 1, value = c(0,1), step=0.001),
            withBusyIndicatorUI(
              actionButton("filter_microbes_prev_btn", "Filter")
            )
          ),

          br(),

          # Reset
          withBusyIndicatorUI(
            actionButton("filter_reset_btn", "Reset")
          ),
          width=5,
          br(),
          downloadButton('download_rds', 'Download Animalcules File'),


          checkboxInput("filter_adv", "Advanced Options"),
          conditionalPanel(
            condition = "input.filter_adv == true",
          # Discard samples
          selectizeInput("filter_sample_dis", "Discard Samples", choices=sam.name, multiple=TRUE),
          # Discard organisms
          selectizeInput("filter_organism_dis", "Discard Organisms", choices=org.name, multiple=TRUE),
          withBusyIndicatorUI(
            actionButton("filter_discard_btn", "Discard")
          )
          ),


          br()

        ),
        mainPanel(
          fluidRow(
            column(5,
              tableOutput("filter_summary_table")
            ),
            column(7,
              plotlyOutput("filter_summary_top_plot", height="350px"),
              plotlyOutput("filter_summary_bottom_plot", height="350px")
            )
          ),
          width=7
        )
      )
    ),
    tabPanel("Categorize",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          selectizeInput('filter_bin_cov', 'Continuous Variables', choices=num_covariates, multiple=FALSE),
          uiOutput("filter_nbins"),

          textInput("filter_new_covariate", "New Variable Label", value = "new_var"),

          checkboxInput("filter_bin_adv", "Advanced Options"),

          conditionalPanel(
            condition = "input.filter_bin_adv == true",
            textInput('filter_bin_breaks', 'Custom Breaks (Comma Delimited)'),
            verbatimTextOutput("filter_bin_to1")
          ),

          conditionalPanel(
            condition = "input.filter_bin_adv == true",
            textInput('filter_bin_labels', 'Custom Labels (Comma Delimited)'),
            verbatimTextOutput("filter_bin_to2")
          ),

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
