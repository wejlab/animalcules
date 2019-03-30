tabPanel("Abundance",
  tabsetPanel(
    tabPanel("Barplots",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          # Sort the samples by a condition
          conditionalPanel(
            condition = "input.relabu_bar_group_samples == false",
            selectizeInput('relabu_bar_sample_conditions', 'Color Samples by Condition', choices=covariates, multiple=TRUE)
          ),
          conditionalPanel(
            condition = "input.relabu_bar_group_samples == true",
            selectizeInput('relabu_bar_group_conditions', 'Color Samples by Condition', choices=c("ALL", covariates))
          ),

          # Sample aggregation
          checkboxInput("relabu_bar_group_samples", "Group Samples by Condition"),

          # Select taxon level
          selectInput("relabu_bar_taxlev", "Tax Level", choices=tax.name, selected=tax.default),

          # Sort the bars
          radioButtons("relabu_bar_sort", "Sort By", c("No Sorting" = "nosort",
                                                           "Conditions" = "conditions",
                                                           "Organisms"  = "organisms"),
                                                           selected     = "nosort"),

          checkboxInput("relabu_bar_adv", "Advanced Options"),

          # Isolate samples
          conditionalPanel(
            condition = "input.relabu_bar_adv == true",
            selectizeInput("relabu_bar_sample_iso", "Isolate Samples", choices=sam.name, multiple=TRUE)
          ),

          # Discard samples
          conditionalPanel(
            condition = "input.relabu_bar_adv == true",
            selectizeInput("relabu_bar_sample_dis", "Discard Samples", choices=sam.name, multiple=TRUE)
          ),

          # Dynamically generate based on tax level
          conditionalPanel(
            condition = "input.relabu_bar_adv == true",
            uiOutput("relabu_bar_org_order")
          ),

          # Legend toggle
          conditionalPanel(
            condition = "input.relabu_bar_adv == true",
            checkboxInput("relabu_bar_legend", "Show Legend", value=TRUE)
          ),

          # Adjust height of plot
          conditionalPanel(
            condition = "input.relabu_bar_adv == true",
            sliderInput("relabu_bar_height", "Plot Height", 600, 1000, value=600, step=50, post="px")
          ),

          # Do plot button
          actionButton("relabu_bar_plot_btn", "Plot"),
          width=3
        ),
        mainPanel(
          uiOutput("relabu_bar_dynamic_plot"),
          width=9
        )
      )
    ),
    tabPanel("Heatmap",
      tags$br(),
      sidebarLayout(
        sidebarPanel(
          # Sort the samples by a condition
          selectizeInput("relabu_heatmap_conditions", "Color Samples by Condition", choices=covariates, multiple=TRUE),

          # Select taxon level
          selectInput("relabu_heatmap_taxlev", "Tax Level", choices=tax.name, selected=tax.default),


          # Column sort
          radioButtons("relabu_heatmap_sort", "Sort By", c("No Sorting" = "nosort",
                                                           "Conditions" = "conditions",
                                                           "Organisms"  = "organisms"),
                                                           selected     = "nosort"),

          checkboxInput("relabu_heatmap_adv", "Advanced Options"),

          # Dynamically generate based on tax level
          conditionalPanel(
            condition = "input.relabu_heatmap_adv == true",
            uiOutput("relabu_heatmap_org_iso")

          ),

          # Isolate samples
          conditionalPanel(
            condition = "input.relabu_heatmap_adv == true",
            selectizeInput("relabu_heatmap_sample_iso", "Isolate Samples", choices=sam.name, multiple=TRUE)

          ),

          # Discard samples
          conditionalPanel(
            condition = "input.relabu_heatmap_adv == true",
            selectizeInput("relabu_heatmap_sample_dis", "Discard Samples", choices=sam.name, multiple=TRUE)
          ),

          # Optional logcpm
          conditionalPanel(
            condition = "input.relabu_heatmap_adv == true",
            checkboxInput("relabu_heatmap_logcpm", "log(CPM)", value=TRUE)
          ),

          # Adjust height of plot
          conditionalPanel(
            condition = "input.relabu_heatmap_adv == true",
            sliderInput("relabu_heatmap_height", "Plot Height", 600, 1000, value=600, step=50, post="px")
          ),

          # Do plot button
          actionButton("relabu_heatmap_plot_btn", "Plot"),
          width=3
        ),
        mainPanel(
          uiOutput("relabu_heatmap_dynamic_plot"),
          width=9
        )
      )
    ),
    tabPanel("Boxplots",
      tags$br(),
      sidebarLayout(
        sidebarPanel(

          selectInput("relabu_box_taxlevs", "Tax Levels", choices=tax.name, selected=tax.default, multiple=TRUE),

          # Dynamic choose from organisms based on tax level
          uiOutput("relabu_box_organisms"),

          # Separate plots
          checkboxInput("relabu_box_separate", "Separate Plots"),

          # Select condition
          selectInput("relabu_box_condition", "Select condition", covariates.colorbar),

          # Select datatype
          radioButtons("relabu_box_datatype", "Select data format", c("Relative Abundance" = "relative abundance",
                                                                      "Counts"             = "counts",
                                                                      "log(CPM)"           = "logcpm"),
                                                                      selected             = "logcpm"),

          # Do plot button
          actionButton("relabu_box_plot_btn", "Plot"),
          width=3
        ),
        mainPanel(
          uiOutput("relabu_box_plots", height=200),
          width=9
        )
      )
    )
  )
)
