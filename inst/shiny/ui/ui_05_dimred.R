tabPanel("Dimension Reduction",
  tabsetPanel(
    tabPanel("PCA",
      tags$br(),
      sidebarLayout(
        sidebarPanel(

          numericInput('dimred_pca_x', 'Principal Component (x-axis)', 1, min = 1, max = 50),

          numericInput('dimred_pca_y', 'Principal Component (y-axis)', 2, min = 1, max = 50),

          selectizeInput('dimred_pca_taxlev', 'Taxonomy Level', choices = tax.name, selected='no rank'),

          selectInput("dimred_pca_color", "Color points by:", covariates),

          selectInput("dimred_pca_shape", "Shape points by:", c("None", covariates.colorbar)),

          selectInput("dimred_pca_datatype", "Select data type", c("Relative Abundance" = "relabu",
                                                                   "Counts"             = "counts",
                                                                   "log(CPM)"           = "logcpm"),
                                                                   selected             = "relabu"),

          # Do plot button
          actionButton("dimred_pca_plot_btn", "Plot"),
          actionButton("dimred_pca_table_btn", "Table"),
          width=3
        ),
        mainPanel(
          fluidRow(
            column(6,
              plotlyOutput("dimred_pca_plot", height="500px")
            ),
            column(6,
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

          numericInput('dimred_pcoa_x', 'Principal Coordinate (x-axis)', 1, min = 1, max = 50),

          numericInput('dimred_pcoa_y', 'Principal Coordinate (y-axis)', 2, min = 1, max = 50),

          selectizeInput('dimred_pcoa_taxlev', 'Taxonomy Level', choices = tax.name, selected='no rank'),

          selectInput("dimred_pcoa_color", "Color points by:", covariates),

          selectInput("dimred_pcoa_shape", "Shape points by:", c("None", covariates.colorbar)),

          selectInput("dimred_pcoa_method", "Select distance method", c("Bray" = "bray"),
                                                                        selected = "bray"),

          # Do plot button
          actionButton("dimred_pcoa_plot_btn", "Plot"),
          actionButton("dimred_pcoa_table_btn", "Table"),
          width=3
        ),
        mainPanel(
          fluidRow(
            column(6,
              plotlyOutput("dimred_pcoa_plot", height="500px")
            ),
            column(6,
              dataTableOutput("dimred_pcoa_table")
            )
          ),
          width=9
        )
      )
    )
  )
)

