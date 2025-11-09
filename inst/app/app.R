library(shiny)
library(ggplot2)
library(DT)
library(readr)
library(readxl)
library(ragg)
library(colourpicker)
library(viridis)
library(dplyr)
library(bslib)
library(clipr)    # for copying code to clipboard (optional)

options(shiny.maxRequestSize = 100*1024^2)  # 100 MB limit


# source modules
source(file.path('R','mod_data.R'))
source(file.path('R','mod_mapping.R'))
source(file.path('R','mod_plot.R'))
source(file.path('R','mod_style.R'))
source(file.path('R','mod_code.R'))
source(file.path('R','mod_export.R'))


ui <- fluidPage(
      theme = bs_theme(bootswatch = "yeti", primary = "#0E1E37"),
      tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "plotarium.css"),
          tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
          tags$script(src = "plotarium.js")

      ),
     div(class = "app-wrapper",
        div(class = "header-row",
            div(class = "header-title", "Plotarium"),
            div(class = "header-logos",
                    img(src = "logo_plotarium_v2.png", alt = "Logo")

            )
        ),
        div(class = "app-body",

            div(class = "main-panels",
                div(class = "panel panel-left",
                    wellPanel(
                        h4("Data & Mapping"),
                        mod_data_ui("data"),
                        hr(),
                        selectInput("plottype", "Plot type:",
                                    choices = c("Scatter", "Boxplot", "Violin", "Histogram", "Bar", "Line", "Tile")),

                        mod_mapping_ui("map"),
                        conditionalPanel(
                            condition = "input.convert_var == true",
                            uiOutput("convert_ui")
                        ),
                        checkboxInput("convert_var", "Convert numeric variable to categorical", value = FALSE),
                        checkboxInput("lazy_render", "Enable lazy rendering for large data (>10k rows)", value = TRUE)

                    )
                ),
                div(class = "panel panel-center",
                    wellPanel(
                        tabsetPanel(
                            tabPanel("Plot", mod_plot_ui("plot")),
                            tabPanel("Interactive", plotly::plotlyOutput("plot_interactive")),
                            tabPanel("Code", mod_code_ui("code")),
                            tabPanel("Data", DT::dataTableOutput("data_preview"))
                        )
                    )
                ),
                div(class = "panel panel-right",
                    wellPanel(
                        h4("Aesthetics & Styling"),
                        mod_style_ui("style"),
                        hr(),
                        mod_export_ui("export")
                    )
                ),
            )
),
tags$footer(
    HTML("For more info visit
           <a href='https://github.com/oli208/plotarium' target='_blank'>
           plotarium on github</a> | &copy; 2025"),
    # Floating theme toggle
    tags$button(
        id = "themeToggle",
        class = "theme-toggle-btn theme-floating",
        title = "Toggle Light/Dark Mode",
        tags$i(class = "theme-icon")
    ),

)
)
)

server <- function(input, output, session) {
  data_r <- mod_data_server("data")
  # convert numeric to factor UI and logic
  observeEvent(data_r(), {
    nums <- names(dplyr::select_if(data_r(), is.numeric))
    output$convert_ui <- renderUI({
      ns <- session$ns
      if (length(nums) == 0) return(tags$div("No numeric variables found."))
      tagList(
        selectInput(ns("convert_var_sel"), "Choose variable to convert:", choices = nums),
        numericInput(ns("convert_bins"), "Number of bins (0 = treat unique values as categories):", value = 0, min = 0, step = 1)
      )
    })
  })

  # mapping module needs raw data and possibly converted dataset.
  data_for_mapping <- reactive({
    df <- data_r()
    req(df)
    if (isTRUE(input$convert_var) && !is.null(input$convert_var_sel) && input$convert_var_sel %in% names(df)) {
      sel <- input$convert_var_sel
      bins <- as.integer(input$convert_bins %||% 0)
      if (bins > 0) {
        df[[sel]] <- cut(df[[sel]], breaks = bins, include.lowest = TRUE, dig.lab = 10)
      } else {
        df[[sel]] <- as.factor(df[[sel]])
      }
    }
    # lazy rendering: if enabled and df large, return sampled df for plotting only (actual data preview still full)
    if (isTRUE(input$lazy_render) && nrow(df) > 10000) {
        shiny::showNotification("Data > 10,000 rows: using 10,000-row sample for plotting.", type = "message", duration = 3)
        df_sample <- df[sample(seq_len(nrow(df)), 10000), , drop = FALSE]
        attr(df_sample, "__sampled") <- TRUE
        return(df_sample)
    }
    df
  })

  mapping_r <- mod_mapping_server("map", data_for_mapping)
  style_r <- mod_style_server("style")
  plot_r <- mod_plot_server("plot", data_for_mapping, mapping_r, reactive(input$plottype), style_r)
  mod_code_server("code", data_for_mapping, mapping_r, reactive(input$plottype), style_r)
  mod_export_server("export", plot_r, style_r)


  # render interactive plotly from the plot reactive
  output$plot_interactive <- plotly::renderPlotly({
      req(plot_r())
      plotly::ggplotly(plot_r(), height = 600)
  })


  output$data_preview <- DT::renderDataTable({
    req(data_for_mapping())
    head(data_for_mapping(), 100)
  })
}

shinyApp(ui, server)
