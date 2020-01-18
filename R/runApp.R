#' Run animalcules shiny app
#'
#' @import assertthat
#' @import covr
#' @import lattice
#' @import DT
#' @importFrom shinyjs addClass
#' @return The shiny app will open
#'
#' @examples 
#' \dontrun{
#' run_animalcules()
#' }
#' @export
run_animalcules <- function() {
    appDir <- system.file("shiny", package = "animalcules")
    if (appDir == "") {
        stop("Could not find myapp. Try re-installing `mypackage`.", 
            call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
