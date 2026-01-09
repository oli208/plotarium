#' Launch Plotarium App
#'
#' @description
#' This function launches the Plotarium Shiny application. It can be run in the background or foreground.
#'
#' @param launch.browser Logical; if TRUE, the app will open in the default web browser.
#' @param background Logical; if TRUE, the app will run in the background using the processx package.
#' @return None; the function launches the Shiny app.
#'
#' @import shiny
#' @importFrom processx process
#'
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
