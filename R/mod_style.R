mod_style_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("labels_cb"), "Change labels (title / x / y)", value = FALSE),
    conditionalPanel("input.labels_cb == true", ns = ns,
                     textInput(ns("title"), "Title", value = ""),
                     textInput(ns("xlabel"), "X-axis label", value = ""),
                     textInput(ns("ylabel"), "Y-axis label", value = "")
    ),
    checkboxInput(ns("theme_cb"), "Change theme", value = FALSE),
    conditionalPanel("input.theme_cb == true", ns = ns,
                     selectInput(ns("theme_sel"), "Theme", choices = c("theme_minimal","theme_bw","theme_classic","theme_light"), selected = "theme_minimal"),
                     sliderInput(ns("base_size"), "Base font size", min = 6, max = 20, value = 11)
    ),
    checkboxInput(ns("legend_cb"), "Legend position", value = TRUE),
    conditionalPanel("input.legend_cb == true", ns = ns,
                     selectInput(ns("legend_pos"), "Legend position", choices = c("right","left","top","bottom","none"), selected = "right")
    ),
    hr(),
    h5("Color scale"),
    selectInput(ns("colorscale"), "Color scale:", choices = c("Default","Viridis","Manual"), selected = "Default"),
    conditionalPanel("input.colorscale == 'Manual'", ns = ns,
                     textInput(ns("manual_palette"), "Manual palette (comma-separated hex, e.g. #1f78b4,#33a02c)", value = "#1f78b4,#33a02c,#e31a1c")
    )
  )
}

mod_style_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(list(
      labels = list(title = input$title, x = input$xlabel, y = input$ylabel),
      labels_enabled = isTRUE(input$labels_cb),
      theme = input$theme_sel,
      theme_enabled = isTRUE(input$theme_cb),
      base_size = input$base_size,
      legend_pos = input$legend_pos,
      legend_enabled = isTRUE(input$legend_cb),
      colorscale = input$colorscale,
      manual_palette = input$manual_palette
    ))
  })
}
