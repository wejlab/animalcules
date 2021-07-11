tabPanel("Pathway Projection",
         sidebarLayout(
           sidebarPanel(
             selectInput("collect", "Select an MSigDB collection:", 
                         choices = names(collections)
                         ),
             uiOutput('subCollect'),
             withBusyIndicatorUI(
               actionButton("path_proj_btn", "Get ssGSEA scores", class="btn-primary" )
               ),
           ),
           mainPanel(
             fluidRow(
               column(12,
                      plotlyOutput("score_heatmap")
                      )
               )
             )
           )
         )