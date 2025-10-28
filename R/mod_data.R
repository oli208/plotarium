mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload data (CSV, Excel, RDS):", accept = c('.csv', '.xlsx', '.xls', '.rds')),
    selectInput(ns("sample"), "Or choose sample dataset:", choices = c("None", "mtcars", "iris"), selected = "mtcars")
  )
}

mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    df <- reactive({
      if (!is.null(input$sample) && input$sample != "None") {
        return(get(input$sample, envir = .GlobalEnv))
      }
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      tryCatch({
        switch(ext,
               csv = readr::read_csv(input$file$datapath),
               xlsx = readxl::read_excel(input$file$datapath),
               xls = readxl::read_excel(input$file$datapath),
               rds = readRDS(input$file$datapath),
               stop("Unsupported file type"))
      }, error = function(e){
        showNotification(paste("Failed to read file:", e$message), type = "error")
        NULL
      })
    })
    return(df)
  })
}
