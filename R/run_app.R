#' Launch Plotarium Addin
#' @export
run_plotarium <- function() {
  app_dir <- system.file("app", package = "plotarium")
  if (app_dir == "" || app_dir == ".") {
    stop("Plotarium app not found in installed package. Please run from package source or install the package.")
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}
