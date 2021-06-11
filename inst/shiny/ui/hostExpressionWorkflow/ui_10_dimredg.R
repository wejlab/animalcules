tabPanel("Dimension Reduction",
         tabsetPanel(
           tabPanel("PCA",
                    tags$br(),
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectInput("dimredg_pca_color", "Color points by:", covariates),
                        
                        checkboxInput("dimredg_pca_adv", "Advanced Options (3D)"),
                        
                        conditionalPanel(
                          condition = "input.dimredg_pca_adv == true | input.global_adv == true",
                          numericInput('dimredg_pca_x', 'Principal Component (x-axis)', 1, min=1, max=50),
                          numericInput('dimredg_pca_y', 'Principal Component (y-axis)', 2, min=1, max=50),
                          numericInput('dimredg_pca_z', 'Principal Component (z-axis)', NA, min=1, max=50),
                          selectInput("dimredg_pca_shape", "Shape points by:", c("None", covariates.colorbar)),
                          selectInput("dimredg_pca_datatype", "Select data type", c("Counts" = "counts",
                                                                                   "log(CPM)" = "logcpm"),
                                      selected = "logcpm")
                        ),
                        
                        # Do plot button
                        withBusyIndicatorUI(
                          actionButton("dimredg_pca_plot_btn", "Plot", class = "btn-primary")
                          ),
                        withBusyIndicatorUI(
                          actionButton("dimredg_pca_table_btn", "Table") 
                          ),
                        width=3
                      ),
                      mainPanel(
                        fluidRow(
                          column(7,
                                 plotlyOutput("dimredg_pca_plot", height="500px")
                          ),
                          column(5,
                                 dataTableOutput("dimredg_pca_table")
                          )
                        ),
                        width=9
                      )
                    )
           ),
           
#          )
# )
           # tabPanel("PCoA",
           #          tags$br(),
           #          sidebarLayout(
           #            sidebarPanel(
           # 
           #              selectInput("dimredg_pcoa_color", "Color points by:", covariates),
           # 
           #              checkboxInput("dimredg_pcoa_adv", "Advanced Options (3D)"),
           # 
           #              conditionalPanel(
           #                condition = "input.dimredg_pcoa_adv == true | input.global_adv == true",
           #                numericInput('dimredg_pcoa_x', 'Principal Coordinate (x-axis)', 1, min=1, max=50),
           #                numericInput('dimredg_pcoa_y', 'Principal Coordinate (y-axis)', 2, min=1, max=50),
           #                numericInput('dimredg_pcoa_z', 'Principal Coordinate (z-axis)', NA, min=1, max=50),
           #                selectInput("dimredg_pcoa_shape", "Shape points by:", c("None", covariates.colorbar)),
           #                selectInput("dimredg_pcoa_method", "Select distance metric",
           #                            c("bray", "jaccard"), selected = "bray")
           #              ),
           # 
           #              # Do plot button
           #              withBusyIndicatorUI(
           #                actionButton("dimredg_pcoa_plot_btn", "Plot", class = "btn-primary") 
           #              ),
           #              withBusyIndicatorUI(
           #                actionButton("dimredg_pcoa_table_btn", "Table") 
           #              ),
           #              width=3
           #            ),
           #            mainPanel(
           #              fluidRow(
           #                column(7,
           #                       plotlyOutput("dimredg_pcoa_plot", height="500px")
           #                ),
           #                column(5,
           #                       dataTableOutput("dimredg_pcoa_table")
           #                )
           #              ),
           #              width=9
           #            )
           #          )
           # )
           tabPanel("UMAP",
                    tags$br(),
                    sidebarLayout(
                      sidebarPanel(

                        selectInput("dimredg_umap_color", "Color points by:", covariates),

                        checkboxInput("dimredg_umap_adv", "Advanced Options (3D)"),

                        conditionalPanel(
                          condition = "input.dimredg_umap_adv == true | input.global_adv == true",
                          numericInput('dimredg_umap_x', 'Component (x-axis)', 1, min=1, max=50),
                          numericInput('dimredg_umap_y', 'Component (y-axis)', 2, min=1, max=50),
                          numericInput('dimredg_umap_z', 'Component (z-axis)', NA, min=1, max=50),
                          numericInput('dimredg_umap_n_neighbors', 'Nearest Neighbors', 2),
                          selectizeInput('dimredg_umap_metric', 'Distance Metric', c("euclidean", "manhattan"), selected="euclidean"),
                          numericInput('dimredg_umap_n_epochs', 'Iterations', 200),
                          selectizeInput('dimredg_umap_init', 'Initial Embedding', c("spectral", "random"), selected="spectral"),
                          numericInput('dimredg_umap_min_dist', 'Min Distance', 0.1),
                          selectInput("dimredg_umap_shape", "Shape points by:", c("None", covariates.colorbar)),
                          selectInput("dimredg_umap_datatype", "Select data type", c("Counts"             = "counts",
                                                                                    "log(CPM)"           = "logcpm"),
                                      selected             = "logcpm")
                        ),

                        # Do plot button
                        actionButton("dimredg_umap_plot_btn", "Plot", class = "btn-primary"),
                        width=3
                      ),
                      mainPanel(
                        plotlyOutput("dimredg_umap_plot", width="700px", height="700px"),
                        width=9
                      )
                    )
           ),
           tabPanel("t-SNE",
                    tags$br(),
                    sidebarLayout(
                      sidebarPanel(

                        selectInput("dimredg_tsne_color", "Color points by:", covariates),

                        checkboxInput("dimredg_tsne_adv", "Advanced Options (3D)"),
                        conditionalPanel(
                          condition = "input.dimredg_tsne_adv == true | input.global_adv == true",
                          checkboxInput("dimredg_tsne_cached", "Use Cached Data", TRUE),
                          helpText("Note: Uncheck the \"Use Cached Data\" first to run 3D tSNE"),
                          selectInput("dimredg_tsne_k", "Select Final Dimensions", c("2D" = "2D",
                                                                                    "3D" = "3D"),
                                      selected = "2D"),
                          selectInput("dimredg_tsne_shape", "Shape points by:", c("None", covariates.colorbar)),
                          helpText("Note: Uncheck the \"Use Cached Data\" first to change perplexity"),
                          numericInput('dimredg_tsne_perplexity', 'Perplexity', 10, min=1, max=1000, step=0.1),
                          numericInput('dimredg_tsne_initial_dims', 'Inital Dimensions', 30, min=10, max=1000),
                        ),

                        # Do plot button
                        withBusyIndicatorUI(
                          actionButton("dimredg_tsne_plot_btn", "Plot",class = "btn-primary")
                        ),
                        width=3
                      ),
                      mainPanel(
                        helpText("Note: it will take longer time for the first run.
         Subsequent plots will use cached assay unless the
         \"Use Cached Data\" is disabled from the advanced option."),
                        fluidRow(
                          plotlyOutput("dimredg_tsne_plot", width="700px", height="700px")
                        ),
                        width=9
                      )
                    )
           )
         )
)
