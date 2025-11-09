mod_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fileInput(
            ns("file"),
            "Upload data (CSV, Excel, RDS):",
            accept = c(".csv", ".xlsx", ".xls", ".rds")
        ),
        
        # sample datasets
        selectInput(ns("sample"), "Or choose a sample dataset:", choices = c("None", "mtcars", "iris"), selected = "None"),
        
        # Excel sheet selector (only visible if Excel file detected)
        conditionalPanel(
            condition = paste0("output['", ns("is_excel"), "']"),
            selectInput(ns("sheet_select"), "(Excel) Sheet:", choices = NULL)
        ),
        
        # CSV import options (only visible if CSV file detected)
        conditionalPanel(
            condition = paste0("output['", ns("is_csv"), "']"),
            selectInput(
                ns("csv_sep"),
                "CSV Separator:",
                choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                selected = ","
            ),
            numericInput(ns("csv_skip"), "Skip lines:", value = 0, min = 0)
        )#,
        
     #   hr(),
     #   selectInput(
     #       ns("data-global"),
     #       "Or choose data.frame from Global Environment:",
     #       choices = c("None")
     #   )
    )
}

mod_data_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        df <- reactiveVal(NULL)
        
        # --- Reactive outputs for conditional panels ---
        output$is_excel <- reactive({
            req(input$file)
            ext <- tools::file_ext(input$file$name)
            ext %in% c("xls", "xlsx")
        })
        outputOptions(output, "is_excel", suspendWhenHidden = FALSE)
        
        output$is_csv <- reactive({
            req(input$file)
            tools::file_ext(input$file$name) == "csv"
        })
        outputOptions(output, "is_csv", suspendWhenHidden = FALSE)
        
        # --- Poll global environment for data.frames ---
        # global_dfs <- reactivePoll(
        #     intervalMillis = 2000,
        #     session,
        #     checkFunc = function() Sys.time(),
        #     valueFunc = function() {
        #         dfs <- ls(envir = .GlobalEnv)
        #         df_choices <- dfs[sapply(dfs, function(x) {
        #             obj <- tryCatch(get(x, envir = .GlobalEnv), error = function(e) NULL)
        #             is.data.frame(obj)
        #         })]
        #         c("None", df_choices)
        #     }
        # )
        # 
        # observe({
        #     updateSelectInput(session, "data-global", choices = global_dfs())
        # })
        
        # --- If user picks a sample dataset, set it immediately ---
        observeEvent(input$sample, {
            if (!is.null(input$sample) && input$sample %in% c("mtcars", "iris")) {
                df(get(input$sample, envir = .GlobalEnv))
            } else if (identical(input$sample, "None")) {
                # do nothing (user can upload or pick global)
            }
        })
        
        # --- Observe file upload ---
        observeEvent(input$file, {
            req(input$file)
            ext <- tools::file_ext(input$file$name)
            
            # Excel: list sheets
            if (ext %in% c("xlsx", "xls")) {
                sheets <- tryCatch(readxl::excel_sheets(input$file$datapath),
                                   error = function(e) NULL)
                if (!is.null(sheets)) {
                    updateSelectInput(session, "sheet_select",
                                      choices = sheets, selected = sheets[1])
                }
            } else {
                updateSelectInput(session, "sheet_select", choices = NULL)
            }
        })
        
        # --- Read data when file or settings change ---
        observeEvent(
            list(input$file, input$sheet_select, input$csv_sep, input$csv_skip),
            {
                # if a sample dataset is selected, prioritize that over file (but global env selection can override)
                if (!is.null(input$sample) && input$sample %in% c("mtcars", "iris")) {
                    return()
                }
                
                req(input$file)
                ext <- tools::file_ext(input$file$name)
                
                df_loaded <- tryCatch({
                    if (ext == "csv") {
                        readr::read_delim(
                            input$file$datapath,
                            delim = input$csv_sep,
                            skip = input$csv_skip,
                            show_col_types = FALSE
                        )
                    } else if (ext %in% c("xlsx", "xls")) {
                        sheet <- if (!is.null(input$sheet_select) &&
                                     input$sheet_select != "") input$sheet_select else 1
                        readxl::read_excel(input$file$datapath, sheet = sheet)
                    } else if (ext == "rds") {
                        readRDS(input$file$datapath)
                    } else {
                        stop("Unsupported file type")
                    }
                }, error = function(e) {
                    showNotification(paste0("Failed to read file: ", e$message), type = "error")
                    NULL
                })
                
                # Sanitize column names to avoid ggplot or mapping errors
                if (!is.null(df_loaded)) {
                    names(df_loaded) <- make.names(names(df_loaded), unique = TRUE)
                }
                
                df(df_loaded)
            },
            ignoreNULL = FALSE
        )
        
        # --- Return logic: priority order ---
        # 1) If global env selection chosen -> return it
        # 2) Else if sample dataset chosen -> return it
        # 3) Else return uploaded df
        reactive({
            # 1) Global env override
            # if (!is.null(input$`data-global`) &&
            #     input$`data-global` != "" &&
            #     input$`data-global` != "None") {
            #     tryCatch({
            #         obj <- get(input$`data-global`, envir = .GlobalEnv)
            #         if (is.data.frame(obj)) return(obj)
            #         NULL
            #     }, error = function(e) NULL)
            # }
            # 2) sample dataset
            if (!is.null(input$sample) && input$sample %in% c("mtcars", "iris")) {
                return(get(input$sample, envir = .GlobalEnv))
            }
            # 3) uploaded
            df()
        })
    })
}
