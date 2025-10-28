mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    h5("Export (high quality)"),
    textInput(ns("fname"), "Filename (without extension)", value = "figure"),
    numericInput(ns("width"), "Width (in)", value = 6, min = 1),
    numericInput(ns("height"), "Height (in)", value = 4, min = 1),
    numericInput(ns("dpi"), "DPI", value = 300, min = 72),
    downloadButton(ns("download_png"), "Download PNG"),
    downloadButton(ns("download_pdf"), "Download PDF")
  )
}

mod_export_server <- function(id, plot_r) {
  moduleServer(id, function(input, output, session) {
    output$download_png <- downloadHandler(
      filename = function(){ paste0(input$fname, ".png") },
      content = function(file){
        ragg::agg_png(file, width = input$width, height = input$height, units = "in", res = input$dpi)
        print(plot_r())
        dev.off()
      }
    )
    output$download_pdf <- downloadHandler(
      filename = function(){ paste0(input$fname, ".pdf") },
      content = function(file){
        grDevices::pdf(file, width = input$width, height = input$height)
        print(plot_r())
        dev.off()
      }
    )
  })
}
