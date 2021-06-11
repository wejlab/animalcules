library(shiny)
library(shinyjs)
library(MultiAssayExperiment)
library(ggplot2)
library(plotly)
library(vegan)
library(dplyr)
library(magrittr)
library(biomformat)
library(heatmaply)
library(DT)
library(msigdbr)
library(GSVA)
library(hypeR)
library(zeallot)


# Load all the functions in /R folder
devtools::load_all("/Users/saketpandit/Documents/BU/Johnson_lab/Asthma/Shiny/animalcules/R")

# full source using local architecture
source(file.path("utils", "helpers.R"),  local = TRUE)
# source(file.path("utils", "server_util.R"),  local = TRUE)
source(file.path("utils", "ui_util.R"),  local = TRUE)

# ui <- navbarPage(
#   title = paste("animalcules v", packageVersion("animalcules"), sep = ""),
#   id="Animalcules",
#   fluid=TRUE,
#   theme = "bootstrap.min.css",
#   source(file.path("ui", "ui_01_upload.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_02_filter.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_03_relabu.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_04_diversity.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_05_dimred.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_06_differential.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_07_biomarker.R"),  local = TRUE)$value,
#   source(file.path("ui", "ui_08_correlations.R"),  local = TRUE)$value
# )

ui <- navbarPage(
  title = paste("animalcules v", packageVersion("animalcules"), sep = ""),
  id="Animalcules",
  fluid=TRUE,
  theme = "bootstrap.min.css",
  source(file.path("ui", "ui_01_upload.R"),  local = TRUE)$value,
  tabPanel("Microbial Abundance Workflow",
           tabsetPanel(
             source(file.path("ui", "microbialWorkflow", "ui_02_filter.R"),  local = TRUE)$value,
             source(file.path("ui", "microbialWorkflow", "ui_03_relabu.R"),  local = TRUE)$value,
             source(file.path("ui", "microbialWorkflow", "ui_04_diversity.R"),  local = TRUE)$value,
             source(file.path("ui", "microbialWorkflow", "ui_05_dimred.R"),  local = TRUE)$value,
             source(file.path("ui", "microbialWorkflow", "ui_06_differential.R"),  local = TRUE)$value,
             source(file.path("ui", "microbialWorkflow", "ui_07_biomarker.R"),  local = TRUE)$value
             )),
  tabPanel("Host Expression Workflow",
           tabsetPanel(
             source(file.path("ui", "hostExpressionWorkflow", "ui_09_pathproj.R"), local = TRUE)$value,
             source(file.path("ui", "hostExpressionWorkflow", "ui_10_dimredg.R"), local = TRUE)$value
             )),
  tabPanel("Integrative Analysis Workflow",
           tabsetPanel(
             source(file.path("ui", "integrativeWorkflow", "ui_08_correlations.R"),  local = TRUE)$value
             ))
  )


server <- function(input, output, session) {
  source(file.path("server", "server_01_upload.R"),  local = TRUE)$value
  source(file.path("server", "microbialWorkflow", "server_02_filter.R"),  local = TRUE)$value
  source(file.path("server", "microbialWorkflow", "server_03_relabu.R"),  local = TRUE)$value
  source(file.path("server", "microbialWorkflow", "server_04_diversity.R"),  local = TRUE)$value
  source(file.path("server", "microbialWorkflow", "server_05_dimred.R"),  local = TRUE)$value
  source(file.path("server", "microbialWorkflow", "server_06_differential.R"),  local = TRUE)$value
  source(file.path("server", "microbialWorkflow", "server_07_biomarker.R"),  local = TRUE)$value
  source(file.path("server", "hostExpressionWorkflow", "server_09_pathproj.R"),  local = TRUE)$value
  source(file.path("server", "hostExpressionWorkflow", "server_10_dimredg.R"),  local = TRUE)$value
  source(file.path("server", "integrativeWorkflow", "server_08_correlations.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
