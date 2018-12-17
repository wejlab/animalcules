#' @export
run_animalcules <- function() {
  appDir <- system.file("shiny", package = "Animalcules")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
