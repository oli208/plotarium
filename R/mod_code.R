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
      aes_line <- if (!is.null(col)) paste0("aes(x = ", x, ", y = ", y, ", color = ", col, ")") else paste0("aes(x = ", x, ", y = ", y, ")")
      geom_line <- switch(ptype,
                          "Scatter" = "geom_point()",
                          "Boxplot" = "geom_boxplot() + geom_jitter(width = 0.15)",
                          "Histogram" = paste0("geom_histogram(bins = 30)"),
                          "Bar" = "geom_bar(position = \"dodge\")",
                          "Line" = "geom_line() + geom_point()",
                          "Tile" = "geom_tile()",
                          "geom_point()")
      lines <- c(lines, paste0("p <- ggplot(", dfname, ", ", aes_line, ") + ", geom_line))
      # facets
  #    if (!is.null(fr) || !is.null(fc)) {
  #      fmla <- paste0(fr %||% ".", " ~ ", fc %||% ".")
  #      lines <- c(lines, paste0("p <- p + facet_grid(", "", fmla, """)))
  #    }
      # theme and labels
      if (isTRUE(st$theme_enabled) && !is.null(st$theme)) {
        lines <- c(lines, paste0("p <- p + ", st$theme, "(base_size = ", st$base_size, ")"))
      } else {
        lines <- c(lines, paste0("p <- p + theme_minimal(base_size = ", st$base_size %||% 11, ")"))
      }
      if (isTRUE(st$labels_enabled)) {
        labs_args <- c()
        if (nzchar(st$labels$title)) labs_args <- c(labs_args, paste0("title = \"", st$labels$title, "\""))
        if (nzchar(st$labels$x)) labs_args <- c(labs_args, paste0("x = \"", st$labels$x, "\""))
        if (nzchar(st$labels$y)) labs_args <- c(labs_args, paste0("y = \"", st$labels$y, "\""))
        lines <- c(lines, paste0("p <- p + labs(", paste(labs_args, collapse = ", "), ")"))
      }
      if (isTRUE(st$legend_enabled)) {
        lines <- c(lines, paste0("p <- p + theme(legend.position = \"", st$legend_pos, "\")"))
      } else {
        lines <- c(lines, "p <- p + theme(legend.position = \"none\")")
      }
      # color scale notes
      if (!is.null(col)) {
        if (st$colorscale == "Viridis") {
          lines <- c(lines, "# apply viridis scale: scale_color_viridis() or scale_fill_viridis() for fill aesthetics")
        } else if (st$colorscale == "Manual") {
          lines <- c(lines, paste0("# manual palette specified: ", st$manual_palette))
          lines <- c(lines, "# use scale_color_manual(values = c(...)) or scale_fill_manual(...) as needed")
        }
      }
      lines <- c(lines, "print(p)", "# ggsave('figure.png', p, width=6, height=4, dpi=300)")
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
