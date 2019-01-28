tabPanel("Dimension Reduction",
  tabsetPanel(
    tabPanel("PCA",
      tags$br(),
      sidebarLayout(
        sidebarPanel(

          selectizeInput('dimred_pca_taxlev', 'Taxonomy Level', choices = tax.name, selected=tax.default),

          selectInput("dimred_pca_color", "Color points by:", covariates),

          checkboxInput("dimred_pca_adv", "Advanced Options (3D)"),

          conditionalPanel(
            condition = "input.dimred_pca_adv == true",
            numericInput('dimred_pca_x', 'Principal Component (x-axis)', 1, min=1, max=50)
          ),
          
          conditionalPanel(
            condition = "input.dimred_pca_adv == true",
            numericInput('dimred_pca_y', 'Principal Component (y-axis)', 2, min=1, max=50)
          ),
          
          conditionalPanel(
            condition = "input.dimred_pca_adv == true",
            numericInput('dimred_pca_z', 'Principal Component (z-axis)', NA, min=1, max=50)
          ),

          conditionalPanel(
            condition = "input.dimred_pca_adv == true",
            selectInput("dimred_pca_shape", "Shape points by:", c("None", covariates.colorbar))
          ),

          conditionalPanel(
            condition = "input.dimred_pca_adv == true",
            selectInput("dimred_pca_datatype", "Select data type", c("Relative Abundance" = "relabu",
                                                                     "Counts"             = "counts",
                                                                     "log(CPM)"           = "logcpm"),
                                                                     selected             = "relabu")
          ),

          # Do plot button
          actionButton("dimred_pca_plot_btn", "Plot"),
          actionButton("dimred_pca_table_btn", "Table"),
          width=3
        ),
        mainPanel(
          fluidRow(
            column(7,
              plotlyOutput("dimred_pca_plot", height="500px")
            ),
            column(5,
              dataTableOutput("dimred_pca_table")
            )
          ),
          width=9
        )
      )
    ),
    tabPanel("PCoA",
      tags$br(),
      sidebarLayout(
        sidebarPanel(

          selectizeInput('dimred_pcoa_taxlev', 'Taxonomy Level', choices = tax.name, selected=tax.default),

          selectInput("dimred_pcoa_color", "Color points by:", covariates),

          checkboxInput("dimred_pcoa_adv", "Advanced Options (3D)"),

          conditionalPanel(
            condition = "input.dimred_pcoa_adv == true",
            numericInput('dimred_pcoa_x', 'Principal Coordinate (x-axis)', 1, min=1, max=50)
          ),
          
          conditionalPanel(
            condition = "input.dimred_pcoa_adv == true",
            numericInput('dimred_pcoa_y', 'Principal Coordinate (y-axis)', 2, min=1, max=50)
          ),
          
          conditionalPanel(
            condition = "input.dimred_pcoa_adv == true",
            numericInput('dimred_pcoa_z', 'Principal Coordinate (z-axis)', NA, min=1, max=50)
          ),
          conditionalPanel(
            condition = "input.dimred_pcoa_adv == true",
            selectInput("dimred_pcoa_shape", "Shape points by:", c("None", covariates.colorbar))
          ),

          conditionalPanel(
            condition = "input.dimred_pcoa_adv == true",
            selectInput("dimred_pcoa_method", "Select distance method", c("Bray" = "bray"),
                                                                          selected = "bray")
          ),

          # Do plot button
          actionButton("dimred_pcoa_plot_btn", "Plot"),
          actionButton("dimred_pcoa_table_btn", "Table"),
          width=3
        ),
        mainPanel(
          fluidRow(
            column(7,
              plotlyOutput("dimred_pcoa_plot", height="500px")
            ),
            column(5,
              dataTableOutput("dimred_pcoa_table")
            )
          ),
          width=9
        )
      )
    ),
    tabPanel("t-SNE",
      tags$br(),
      sidebarLayout(
        sidebarPanel(

          selectizeInput('dimred_tsne_taxlev', 'Taxonomy Level', choices = tax.name, selected=tax.default),

          selectInput("dimred_tsne_color", "Color points by:", covariates),

          checkboxInput("dimred_tsne_adv", "Advanced Options (3D)"),

          conditionalPanel(
            condition = "input.dimred_tsne_adv == true",
            selectInput("dimred_tsne_k", "Select Final Dimensions", c("2D" = "2D",
                                                                      "3D" = "3D"),
                                                                      selected = "2D")
          ),

          conditionalPanel(
            condition = "input.dimred_tsne_adv == true",
            selectInput("dimred_tsne_shape", "Shape points by:", c("None", covariates.colorbar))
          ),

          conditionalPanel(
            condition = "input.dimred_tsne_adv == true",
            numericInput('dimred_tsne_perplexity', 'Perplexity', 10, min=1, max=1000, step=0.1)
          ),

          conditionalPanel(
            condition = "input.dimred_tsne_adv == true",
            numericInput('dimred_tsne_initial_dims', 'Inital Dimensions', 30, min=10, max=1000)
          ),

          conditionalPanel(
            condition = "input.dimred_tsne_adv == true",
            selectInput("dimred_tsne_datatype", "Select data type", c("Relative Abundance" = "relabu",
                                                                     "Counts"              = "counts",
                                                                     "log(CPM)"            = "logcpm"),
                                                                     selected              = "relabu")
          ),

          # Do plot button
          actionButton("dimred_tsne_plot_btn", "Plot"),
          width=3
        ),
        mainPanel(
          fluidRow(
            plotlyOutput("dimred_tsne_plot", height="500px", width="500px")
          ),
          width=9
        )
      )
    )
  )
)
