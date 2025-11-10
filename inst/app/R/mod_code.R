mod_code_ui <- function(id) {
    ns <- NS(id)
    tagList(
        verbatimTextOutput(ns("code")),
        downloadButton(ns("download_code"), "Download R code")
    )
}

mod_code_server <- function(id, data_r, mapping_r, plottype_r, style_r) {
    moduleServer(id, function(input, output, session) {
        code_text <- reactive({
            req(mapping_r()$x)
            dfname <- "data"
            x <- mapping_r()$x
            y <- mapping_r()$y
            col <- mapping_r()$color
            fr <- mapping_r()$facet_row
            fc <- mapping_r()$facet_col
            ptype <- plottype_r()
            st <- style_r()

            lines <- c("library(ggplot2)", "# assume 'data' contains your dataframe")
            aes_line <- if (!is.null(col) && !is.null(y)) paste0("aes(x = ", x, ", y = ", y, ", color = ", col, ")") else if (!is.null(col) && is.null(y)) paste0("aes(x = ", x, ", fill = ", col, ")") else if (!is.null(y)) paste0("aes(x = ", x, ", y = ", y, ")") else paste0("aes(x = ", x, ")")
            geom_line <- switch(ptype,
                                "Scatter" = "geom_point()" ,
                                "Boxplot" = "geom_boxplot() + geom_jitter(width = 0.15)",
                                "Violin" = "geom_violin(trim = FALSE)",
                                "Histogram" = "geom_histogram(bins = 30)" ,
                                "Bar" = "geom_bar(position = \"dodge\")",
                                "Line" = "geom_line() + geom_point()",
                                "geom_point()")
            lines <- c(lines, paste0("p <- ggplot(", dfname, ", ", aes_line, ") + ", geom_line))

            # regression
            if (ptype == "Scatter" && isTRUE(mapping_r()$show_regline)) {
                method <- mapping_r()$reg_method
                if (is.null(method) || method == "") method <- "lm"
                lines <- c(lines, paste0("p <- p + geom_smooth(method = '", method, "', se = ", ifelse(isTRUE(mapping_r()$show_conf), "TRUE", "FALSE"), ")"))
            }

            # jitter for box/violin
            if (ptype %in% c("Boxplot","Violin") && isTRUE(mapping_r()$show_jitter)) {
                lines <- c(lines, paste0("p <- p + geom_jitter(width = 0.15, size = ", mapping_r()$jitter_size, ", alpha = 0.6)"))
            }

            # facets
            if (!is.null(fr) || !is.null(fc)) {
                frs <- if (!is.null(fr)) fr else "."
                fcs <- if (!is.null(fc)) fc else "."
                fmla <- paste0(frs, " ~ ", fcs)
                if (!is.null(st$facet_scales)) {
                    lines <- c(lines, paste0("p <- p + facet_grid(", "", fmla, ", scales = \"", st$facet_scales, "\"", ")") )
                } else {
                    lines <- c(lines, paste0("p <- p + facet_grid(", "", fmla, "") )
                }
            }

            # theme and labels (axis sizes)
            lines <- c(lines, paste0("p <- p + theme_minimal() + theme(axis.title = element_text(size = ", style_r()$axis_title_size, "), axis.text = element_text(size = ", style_r()$axis_text_size, "))"))
            if (isTRUE(st$labels_enabled)) {
                labs_args <- c()
                if (!is.null(st$labels$title) && nzchar(st$labels$title)) labs_args <- c(labs_args, paste0("title = \"", st$labels$title, "\""))
                if (!is.null(st$labels$x) && nzchar(st$labels$x)) labs_args <- c(labs_args, paste0("x = \"", st$labels$x, "\""))
                if (!is.null(st$labels$y) && nzchar(st$labels$y)) labs_args <- c(labs_args, paste0("y = \"", st$labels$y, "\""))
                lines <- c(lines, paste0("p <- p + labs(", paste(labs_args, collapse = ", "), ")"))
            }

            lines <- c(lines, "print(p)")
            paste(lines, collapse = "\n")
        })

        output$code <- renderText({ code_text() })

        output$download_code <- downloadHandler(
            filename = function(){ "plot_code.R" },
            content = function(file){
                writeLines(code_text(), con = file)
            }
        )
    })
}
