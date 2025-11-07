mod_mapping_ui <- function(id) {
    ns <- NS(id)
    tagList(

        selectInput(ns("xvar"), "X variable", choices = NULL),
        selectInput(ns("yvar"), "Y variable", choices = NULL),
        selectInput(ns("colorvar"), "Color", choices = c("None")),
        selectInput(ns("facet_row"), "Facet row", choices = c("None")),
        selectInput(ns("facet_col"), "Facet column", choices = c("None")),
        hr(),
        conditionalPanel("input.plottype == 'Scatter'",
                         checkboxInput(ns("show_regline"), "Show regression line", value = FALSE),
                         conditionalPanel("input.show_regline == true", ns = ns,
                                          selectInput(ns("reg_method"), "Regression method:", choices = c("lm","loess"), selected = "lm"),
                                          checkboxInput(ns("show_conf"), "Show confidence interval", value = TRUE)
                         )
        ),
        conditionalPanel("input.plottype == 'Boxplot' || input.plottype == 'Violin'",
                         checkboxInput(ns("show_jitter"), "Show jitter points", value = FALSE),
                         conditionalPanel("input.show_jitter == true", ns = ns,
                                          sliderInput(ns("jitter_size"), "Point size", min = 0.5, max = 6, value = 2)
                         )
        )
    )
}

mod_mapping_server <- function(id, data_r) {
    moduleServer(id, function(input, output, session) {
        observeEvent(data_r(), {
            df <- data_r()
            vars <- names(df)
            updateSelectInput(session, "xvar", choices = vars, selected = vars[1])
            updateSelectInput(session, "yvar", choices = vars, selected = vars[min(2, length(vars))])
            updateSelectInput(session, "colorvar", choices = c("None", vars), selected = "None")
            updateSelectInput(session, "facet_row", choices = c("None", vars), selected = "None")
            updateSelectInput(session, "facet_col", choices = c("None", vars), selected = "None")
        }, ignoreNULL = TRUE)
        
        reactive(list(
            x = input$xvar,
            y = input$yvar,
            color = if (is.null(input$colorvar) || input$colorvar == "None") NULL else input$colorvar,
            facet_row = if (is.null(input$facet_row) || input$facet_row == "None") NULL else input$facet_row,
            facet_col = if (is.null(input$facet_col) || input$facet_col == "None") NULL else input$facet_col,
            show_regline = isTRUE(input$show_regline),
            reg_method = input$reg_method,
            show_conf = isTRUE(input$show_conf),
            show_jitter = isTRUE(input$show_jitter),
            jitter_size = input$jitter_size
        ))
    })
}
