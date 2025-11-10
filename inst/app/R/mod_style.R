# plotarium
mod_style_ui <- function(id) {
    ns <- NS(id)
    tagList(
        checkboxInput(ns("labels_cb"), "Change labels (title / x / y)", value = FALSE),
        conditionalPanel("input.labels_cb == true",
            ns = ns,
            textInput(ns("title"), "Title", value = ""),
            textInput(ns("xlabel"), "X-axis label", value = ""),
            textInput(ns("ylabel"), "Y-axis label", value = "")
        ),
        checkboxInput(ns("theme_cb"), "Change theme", value = FALSE),
        conditionalPanel("input.theme_cb == true",
            ns = ns,
            selectInput(ns("theme_sel"), "Theme", choices = c("theme_minimal", "theme_bw", "theme_classic", "theme_light"), selected = "theme_minimal")
        ),
        checkboxInput(ns("text_sizes_cb"), "Change Font size", value = FALSE),
        conditionalPanel("input.text_sizes_cb == true",
            ns = ns,
            sliderInput(ns("axis_title_size"), "Axis title size", min = 8, max = 30, value = 16),
            sliderInput(ns("axis_text_size"), "Axis text size", min = 6, max = 24, value = 12)
        ),
        checkboxInput(ns("legend_cb"), "Legend position", value = TRUE),
        conditionalPanel("input.legend_cb == true",
            ns = ns,
            selectInput(ns("legend_pos"), "Legend position", choices = c("right", "left", "top", "bottom", "none"), selected = "right")
        ),
        hr(),
        h5("Color scale"),
        selectInput(ns("colorscale"), "Color scale:", choices = c("Default", "Viridis", "Manual"), selected = "Default"),
        conditionalPanel("input.colorscale == 'Manual'",
            ns = ns,
            textInput(ns("manual_palette"), "Manual palette (comma-separated hex, e.g. #1f78b4,#33a02c)", value = "#1f78b4,#33a02c,#e31a1c")
        ),
        hr(),
    #    conditionalPanel("input.facet_row != 'None' || input.facet_col != 'None'",
        selectInput(ns("facet_scales"), "Facet scales:", choices = c("fixed","free"), selected = "fixed"),
   #     ),
        hr(),

        # color blind check
        checkboxInput(ns("cb_sim"), "Enable color blindness preview", value = FALSE),
        conditionalPanel("input.cb_sim == true", ns = ns,
                         selectInput(ns("cb_type"), "Type of color blindness:",
                                     choices = c("deuteranope","protanope","desaturate"),
                                     selected = "deuteranope")
        ),

        hr(),
        h5("Export Settings"),
        selectInput(ns("export_type"), "File type:", choices = c("PNG", "PDF", "SVG"), selected = "PNG"),
        numericInput(ns("export_width_cm"), "Width (cm):", value = 20, min = 1),
        numericInput(ns("export_height_cm"), "Height (cm):", value = 15, min = 1),
        numericInput(ns("export_dpi"), "DPI:", value = 300, min = 72)
    )
}

mod_style_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        reactive(list(
            labels = list(title = input$title, x = input$xlabel, y = input$ylabel),
            labels_enabled = isTRUE(input$labels_cb),
            theme = input$theme_sel,
            theme_enabled = isTRUE(input$theme_cb),
            axis_text_sizes_enabled = isTRUE(input$text_sizes_cb),
            axis_title_size = input$axis_title_size,
            axis_text_size = input$axis_text_size,
            legend_pos = input$legend_pos,
            legend_enabled = isTRUE(input$legend_cb),
            colorscale = input$colorscale,
            manual_palette = input$manual_palette,
            facet_scales = input$facet_scales,
            cb_sim = isTRUE(input$cb_sim),
            cb_type = input$cb_type,
            export = list(type = input$export_type, width_cm = input$export_width_cm, height_cm = input$export_height_cm, dpi = input$export_dpi) # ,
            #        copy_code = input$copy_code
        ))
    })
}
