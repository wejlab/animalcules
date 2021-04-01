tabPanel("GSVA",
         sidebarLayout(
           sidebarPanel(
             selectInput("collect", "Select an MSigDB collection:", 
                         choices = collections
                         ),
             uiOutput('subCollect'),
             # conditionalPanel(
             #   condition = paste0("[", paste0("'",names(sub_collections), sep="", collapse="',"), "'].includes(input.collect) == true"),
             #   selectInput("subCollect", "Select a sub-collection:",
             #               choices = names(sub_collections))
             #   ),
             # ,
             # conditionalPanel(
             #   condition = "input['collect'] == 'C3'",
             #   selectInput("subCollect", "Select a sub-collection:",
             #               choices = sub_collections["C3"][[1]])
             #   ),
             # conditionalPanel(
             #   condition = "input['collect'] == 'C4'",
             #   selectInput("subCollect", "Select a sub-collection:",
             #               choices = sub_collections["C4"][[1]])
             #   ),
             # conditionalPanel(
             #   condition = "input['collect'] == 'C5'",
             #   selectInput("subCollect", "Select a sub-collection:",
             #               choices = sub_collections["C5"][[1]])
             #   ),
             withBusyIndicatorUI(
               actionButton("dimred_gsva_btn", "Get ssGSEA scores")
               ),
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Data summary",
                        dataTableOutput("genes_dimsred_gsva") # Showing the data
                        )
             )
             )
           )
         )