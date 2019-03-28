tabPanel("Upload",
useShinyjs(),
tags$style(appCSS),
tags$div(
    class = "jumbotron",
    tags$div(
        class = "container",
        fluidRow(
            column(7, h1("animalcules")),
            column(5, br(),br(),img(src = "bu_bioinfo_logo.png", height = 100, width = 100))

        ),
        p("Statistical Microbiome Analysis Toolkit"),
        uiOutput("tab")

    )
),
sidebarLayout(
   sidebarPanel(
       radioButtons("uploadChoice", "Upload:",
                    c("example data" = "example",
                      "count file" = "count",
                      "pathoScope file" = "pathofiles",
                      "animalcules-id file" = "animalcules-id",
                      "animalcules file" = "animalcules.file"
                    )),
       conditionalPanel(condition = sprintf("input['%s'] == 'example'", "uploadChoice"),
                          selectInput("example_data", "Example dataset",
              c("Toy dataset" = "toy",
                "TB dataset" = "tb"
                )),
                        withBusyIndicatorUI(
                          actionButton("upload_example",
                                       "Upload",
                                       class = "btn-primary")
                        )
       ),
       conditionalPanel(condition = sprintf("input['%s'] == 'animalcules.file'", "uploadChoice"),
                        fileInput("rdfile", ".rds file (required):",
                                  accept = c(
                                    ".rds"
                                  )
                        ),
                        radioButtons("rdtype", "Filetype",
                                     choices = c(
                                                 rds = "rds"
                                     ),
                                     selected = "rds"
                        ),
                        withBusyIndicatorUI(
                          actionButton("upload_animalcules",
                                       "Upload",
                                       class = "btn-primary")
                        )

       ),
       conditionalPanel(condition = sprintf("input['%s'] == 'animalcules-id'", "uploadChoice"),
                        fileInput("rdfile_id", ".rds file (required):",
                                  accept = c(
                                    ".rds"
                                  )
                        ),
                        radioButtons("mae_data_type", "Choose count type",
                                     choices = c(
                                                 "EM count" = "em",
                                                 "Best hit" = 'hit'
                                     )
                        ),
                        withBusyIndicatorUI(
                          actionButton("upload_mae",
                                       "Upload",
                                       class = "btn-primary")
                        )

       ),
       conditionalPanel(condition = sprintf("input['%s'] == 'count'", "uploadChoice"),
                        fileInput("countsfile", "Counts .csv file (required):",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values",
                                      "text/tab-separated-values",
                                      "text/plain",
                                      ".csv",
                                      ".tsv"
                                  )
                        ),
                        fileInput("taxon.table", "Taxonomy table .csv file (required):",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values",
                                    "text/tab-separated-values",
                                    "text/plain",
                                    ".csv",
                                    ".tsv"
                                  )
                        ),
                        fileInput("annotfile.count", "Annotation .csv file (required):",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values",
                                      "text/tab-separated-values",
                                      "text/plain",
                                      ".csv",
                                      ".tsv"
                                  )
                        ),
                        numericInput("metadata_sample_name_col_count", "Which column in metadata is sample name?",
                                     value = 1),
                        # Input: Checkbox if file has header ----
                        checkboxInput("header.count", "Header", TRUE),

                        # Input: Select separator ----
                        radioButtons("sep.count", "Separator",
                                     choices = c(Tab = "\t",
                                                 Comma = ",",
                                                 Semicolon = ";"
                                     ),
                                     selected = ","),
                        withBusyIndicatorUI(
                            actionButton("uploadDataCount",
                                         "Upload",
                                         class = "btn-primary")
                        ),
                        helpText("After click Upload, please wait until seeing a green check.")
       ),
       conditionalPanel(condition = sprintf("input['%s'] == 'pathofiles'", "uploadChoice"),
                        h5("Upload PathoScope generated .tsv files:"),
                        fileInput("countsfile.pathoscope", "PathoScope outputs (required):",
                                  multiple = TRUE,
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values",
                                      "text/tab-separated-values",
                                      "text/plain",
                                      ".csv",
                                      ".tsv"
                                  )
                        ),
                        fileInput("annotfile.ps", "Annotation file (required):",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values",
                                      "text/tab-separated-values",
                                      "text/plain",
                                      ".csv",
                                      ".tsv"
                                  )
                        ),
                        textInput("report_suffix", "Report suffix", value = "-sam-report.tsv"),
                        numericInput("metadata_sample_name_col", "Which column in metadata is sample name?",
                                     value = 1),
                        # Input: Checkbox if file has header ----
                        checkboxInput("header.ps", "Header", TRUE),

                        # Input: Select separator ----
                        radioButtons("sep.ps", "Separator",
                                     choices = c(Tab = "\t",
                                                 Comma = ",",
                                                 Semicolon = ";"
                                     ),
                                     selected = "\t"),
                        withBusyIndicatorUI(
                            actionButton("uploadDataPs",
                                         "Upload",
                                         class = "btn-primary")
                        ),
                        helpText("After click, please wait for 30s until seeing a green check.")

       )
   ),
   mainPanel(
       conditionalPanel(condition = "input.uploadChoice === 'pathofiles'",
                        h4("Please click \"open in browser\" for enabling functions like multiple files upload."),
                        helpText("Counts Table: column names must be sample name"),
                        DT::dataTableOutput("contents.count"),
                        helpText("Annotation table"),
                        DT::dataTableOutput("contents.meta")
       ),
       conditionalPanel(condition = "input.uploadChoice === 'count'",

                        tags$img(src='count_table_example.png', height = 180, width = 800),
                        helpText("Counts Table: column names must be sample name"),
                        helpText("The first column must be microbe name"),

                        DT::dataTableOutput("contents.count.2"),
                        helpText("Taxonomy Table: column names must be taxonomy levels, like family, genus, species..."),
                        helpText("The first column must be microbe name"),

                        DT::dataTableOutput("contents.taxonomy"),
                        helpText("Annotation table"),

                        DT::dataTableOutput("contents.meta.2")
        )
      )
   )
)
