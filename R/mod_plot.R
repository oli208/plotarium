mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"), height = "600px"),
    br(),
    downloadButton(ns("download"), "Download PNG (preview size)")
  )
}

mod_plot_server <- function(id, data_r, mapping_r, plottype_r, style_r) {

    moduleServer(id, function(input, output, session) {
        
        plot_reactive <- reactive({
            req(data_r(), mapping_r()$x)
            df <- data_r()
            x <- mapping_r()$x
            y <- mapping_r()$y
            col <- mapping_r()$color
            facet_row <- mapping_r()$facet_row
            facet_col <- mapping_r()$facet_col
            ptype <- plottype_r()
            st <- style_r()
            
            # build aes depending on presence of color and y
            if (!is.null(col) && !is.null(y)) {
                aes_args <- aes_string(x = x, y = y, color = col)
            } else if (!is.null(col) && is.null(y)) {
                aes_args <- aes_string(x = x, fill = col)
            } else if (is.null(col) && !is.null(y)) {
                aes_args <- aes_string(x = x, y = y)
            } else {
                aes_args <- aes_string(x = x)
            }
            
            p <- switch(ptype,
                        "Scatter" = {
                            p0 <- ggplot(df, aes_string(x = x, y = y, color = col))
                            p0 <- p0 + geom_point()
                            if (isTRUE(mapping_r()$show_regline)) {
                                method <- mapping_r()$reg_method
                                if (is.null(method) || method == "") method <- "lm"
                                p0 <- p0 + geom_smooth(method = method, se = isTRUE(mapping_r()$show_conf))
                            }
                            p0
                        },
                        "Boxplot" = {
                            req(y)
                            p0 <- ggplot(df, aes_string(x = x, y = y, color = col)) + geom_boxplot(outlier.shape = NA)
                            if (isTRUE(mapping_r()$show_jitter)) {
                                p0 <- p0 + geom_jitter(width = 0.15, size = mapping_r()$jitter_size, alpha = 0.6)
                            }
                            p0
                        },
                        "Violin" = {
                            req(y)
                            p0 <- ggplot(df, aes_string(x = x, y = y, fill = col)) + geom_violin(trim = FALSE)
                            if (isTRUE(mapping_r()$show_jitter)) {
                                p0 <- p0 + geom_jitter(width = 0.15, size = mapping_r()$jitter_size, alpha = 0.6)
                            }
                            p0
                        },
                        "Histogram" = {
                            ggplot(df, aes_string(x = x, fill = col)) + geom_histogram(bins = 30, alpha = 0.8)
                        },
                        "Bar" = {
                            ggplot(df, aes_string(x = x, fill = col)) + geom_bar(position = "dodge")
                        },
                        "Line" = {
                            req(y)
                            ggplot(df, aes_string(x = x, y = y, color = col, group = col)) + geom_line() + geom_point()
                        },
                        "Tile" = {
                            if (is.null(y)) stop("Tile plot needs a Y variable")
                            df %>%
                                select(all_of(c(x, y, col))) %>%
                                na.omit() %>%
                                ggplot(aes_string(x = x, y = y, fill = col)) + geom_tile()
                        },
                        ggplot(df, aes_args) + geom_point()
            )
            
            # facets (use facet_grid with formula)
            if (!is.null(facet_row) || !is.null(facet_col)) {
                fr <- if (!is.null(facet_row)) facet_row else "."
                fc <- if (!is.null(facet_col)) facet_col else "."
                fmla <- paste0(fr, " ~ ", fc)
                p <- p + facet_grid(as.formula(fmla))
            }
            
            # theme and text sizes (axis title/text sizes apply to text only)
            if (isTRUE(st$theme_enabled) && !is.null(st$theme)) {
                theme_fun <- match.fun(st$theme)
                p <- p + theme_fun()
            } else {
                p <- p + theme_minimal()
            }
            
            if(isTRUE(st$axis_text_sizes_enabled)){
                p <- p + theme(axis.title = element_text(size = st$axis_title_size),
                               axis.text = element_text(size = st$axis_text_size))
            }

            # labels
            if (isTRUE(st$labels_enabled)) {
                labs_args <- list()
                if (!is.null(st$labels$title) && nzchar(st$labels$title)) labs_args$title <- st$labels$title
                if (!is.null(st$labels$x) && nzchar(st$labels$x)) labs_args$x <- st$labels$x
                if (!is.null(st$labels$y) && nzchar(st$labels$y)) labs_args$y <- st$labels$y
                if (length(labs_args) > 0) p <- p + do.call(ggplot2::labs, labs_args)
            }
            
            # legend
            if (isTRUE(st$legend_enabled) && !is.null(st$legend_pos)) {
                p <- p + theme(legend.position = st$legend_pos)
            } else {
                p <- p + theme(legend.position = "none")
            }
            
    })

    output$plot <- renderPlot({
      p <- plot_reactive()
      req(p)
      print(p)
    })

    output$download <- downloadHandler(
      filename = function() { paste0("plot_preview.png") },
      content = function(file) {
        ragg::agg_png(file, width = 8, height = 6, units = "in", res = 300)
        print(plot_reactive())
        dev.off()
      }
    )

    return(plot_reactive)
  })
}
