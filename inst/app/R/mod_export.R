mod_export_ui <- function(id) {
    ns <- NS(id)
    tagList(
        downloadButton(ns("export_file"), "Export plot")
    )
}

mod_export_server <- function(id, plot_r, style_r) {
    moduleServer(id, function(input, output, session) {

        output$export_file <- downloadHandler(
            filename = function() {
                req(style_r())
                st <- style_r()
                ext <- tolower(st$export$type)
                paste0("plot_export.", ext)
            },

            content = function(file) {
                req(plot_r(), style_r())
                p <- plot_r()
                st <- style_r()

                w <- st$export$width_cm / 2.54
                h <- st$export$height_cm / 2.54
                dpi <- st$export$dpi
                type <- toupper(st$export$type)

                withCallingHandlers({
                    if (type == "PNG") {
                        ragg::agg_png(file, width = w, height = h, units = "in", res = dpi)
                        print(p)
                        dev.off()

                    } else if (type == "PDF") {
                        grDevices::pdf(file, width = w, height = h)
                        print(p)
                        dev.off()

                    } else if (type == "SVG") {
                        grDevices::svg(file, width = w, height = h)
                        print(p)
                        dev.off()

                    } else {
                        # fallback
                        ragg::agg_png(file, width = w, height = h, units = "in", res = dpi)
                        print(p)
                        dev.off()
                    }
                }, error = function(e) {
                    showNotification(paste("Export failed:", e$message), type = "error")
                })
            }
        )
    })
}
