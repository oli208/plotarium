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

options(shiny.maxRequestSize = 100*1024^2)  # 100 MB limit


# source modules
source("R/mod_data.R")
source("R/mod_mapping.R")
source("R/mod_plot.R")
source("R/mod_style.R")
source("R/mod_code.R")
source("R/mod_export.R")


ui <- fluidPage(
      theme = bs_theme(bootswatch = "yeti", primary = "#1f77b4"),
    
    tags$head(
        tags$style(HTML("
      /* Include padding/border in width calculation */
      *, *::before, *::after {
        box-sizing: border-box;
      }

      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow-x: hidden; /* Hide horizontal scroll */
      }

      .container-fluid {
        height: 100vh;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }

      .app-wrapper {
        display: flex;
        flex-direction: column;
        height: 100%;
      }

      .app-body {
        flex: 1;
        overflow-y: auto;
        overflow-x: hidden; /* Hide horizontal scroll */
      }

      .header-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 8px 16px;
        border-bottom: 1px solid #ccc;
        background-color: #ffffff;
        flex-shrink: 0;
        max-width: 100vw; /* prevent overflow */
      }

      .header-title {
        font-size: 22px;
        font-weight: 600;
        color: #333;
        margin: 0;
      }

      .header-logos {
        display: flex;
        gap: 5px;
        max-width: 50vw;
      }

      .header-logos img {
        height: 36px;
        width: auto;
        max-width: 100%;
        display: block;
      }

      footer {
        background-color:#f8f9fa;
        padding: 10px 16px;
        border-top: 1px solid #ccc;
        text-align:center;
        font-size:11px;
        color:#555;
        flex-shrink: 0;
        max-width: 100vw;
      }

      /* Sidebar and main panel tweaks */
      .sidebar {
        padding-right: 0;
      }

      .main-panel {
        padding-left: 0;
        max-width: 100vw;
      }
    "))
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
                        mod_mapping_ui("map"),
                        hr(),
                        selectInput("plottype", "Plot type:", 
                                    choices = c("Scatter", "Boxplot", "Histogram", "Bar", "Line", "Tile")),
                        checkboxInput("convert_var", "Convert numeric variable to categorical", value = FALSE),
                        conditionalPanel(
                            condition = "input.convert_var == true",
                            uiOutput("convert_ui")
                        )
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
            
    # column(width = 6,
    #        wellPanel(
    #            tabsetPanel(
    #                tabPanel("Plot", mod_plot_ui("plot")),
    #                tabPanel("Code", mod_code_ui("code")),
    #                tabPanel("Data", DT::dataTableOutput("data_preview"))
    #            )
    #        )
           
           
           # wellPanel(
           #   h4("Preview"),
           #   mod_plot_ui("plot"),
           #   br(),
           #   tabsetPanel(
           #     tabPanel("Code", mod_code_ui("code")),
           #     tabPanel("Data", DT::dataTableOutput("data_preview"))
           #   )
           # )
    ),
    column(width = 3,
           wellPanel(
             h4("Aesthetics & Styling"),
             mod_style_ui("style")
           )
    )
  )
),
tags$footer(
    HTML("For more info visit 
           <a href='https://github.com/oli208/plotarium' target='_blank'>
           plotarium on github</a> | &copy; 2025")
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
    df
  })

  mapping_r <- mod_mapping_server("map", data_for_mapping)
  style_r <- mod_style_server("style")
  plot_r <- mod_plot_server("plot", data_for_mapping, mapping_r, reactive(input$plottype), style_r)
  mod_code_server("code", data_for_mapping, mapping_r, reactive(input$plottype), style_r)
  mod_export_server("export", plot_r)

  
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
