#' Find the taxonomy for maximum 300 tids
#'
#' @export
#' @examples
#' run_animalcules()

run_animalcules <- function() {
  appDir <- system.file("shiny", package = "animalcules")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
