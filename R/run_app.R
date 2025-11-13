#' Launch Plotarium App
#' @export
# run_plotarium <- function() {
#   app_dir <- system.file("app", package = "plotarium")
#   if (app_dir == "" || app_dir == ".") {
#     stop("Plotarium app not found in installed package. Please run from package source or install the package.")
#   }
#   shiny::runApp(app_dir, launch.browser = TRUE)
# }


run_plotarium <- function(launch.browser = TRUE, background = TRUE) {
    app_dir <- system.file("app", package = "plotarium")
    if (app_dir == "" || app_dir == ".") {
        stop("Plotarium app not found in installed package. Please run from package source or install the package.")
    }

    if (background) {
        processx::process$new(
            "R",
            c("-e", sprintf("shiny::runApp('%s', launch.browser = %s)", app_dir, launch.browser)),
            stdout = "|", stderr = "|"
        )
        message("🚀 Plotarium launched in the background.")
    } else {
        shiny::runApp(app_dir, launch.browser = launch.browser)
    }
}
