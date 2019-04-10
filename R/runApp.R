#' Find the taxonomy for maximum 300 tids
#'
#' @return None
#' @import assertthat
#' @import shiny
#' @import tibble
#' @import ape
#' @import vegan
#' @import S4Vectors
#' @import scales
#' @import covr
#' @import lattice
#' @import DT
#' @importFrom shinyjs addClass
#' @export
run_animalcules <- function() {
  appDir <- system.file("shiny", package = "animalcules")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
