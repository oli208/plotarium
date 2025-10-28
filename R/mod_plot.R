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

      aes_args <- if (!is.null(col)) aes_string(x = x, y = y, color = col) else aes_string(x = x, y = y)

      # build base plot by type
      p <- switch(ptype,
                  "Scatter" = ggplot(df, aes_args) + geom_point(),
                  "Boxplot" = {
                    if (is.null(y)) stop("Boxplot needs a Y variable")
                    ggplot(df, aes_string(x = x, y = y, color = col)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, height = 0, alpha = 0.6)
                  },
                  "Histogram" = {
                    if (is.null(x)) stop("Histogram needs X variable") 
                    ggplot(df, aes_string(x = x, fill = col)) + geom_histogram(bins = 30, alpha = 0.8)
                  },
                  "Bar" = {
                    # treat x as categorical
                    ggplot(df, aes_string(x = x, fill = col)) + geom_bar(position = "dodge")
                  },
                  "Line" = {
                    # group by color if present or by x otherwise
                    grp <- if (!is.null(col)) col else NULL
                    ggplot(df, aes_string(x = x, y = y, color = col, group = grp)) + geom_line() + geom_point()
                  },
                  ggplot(df, aes_args) + geom_point()
      )

      # facets
      if (!is.null(facet_row) || !is.null(facet_col)) {
        fr <- if (!is.null(facet_row)) paste0("vars(`", facet_row, "`)") else "."
        fc <- if (!is.null(facet_col)) paste0("vars(`", facet_col, "`)") else "."
        # safer: use facet_grid with formula
        fmla <- paste0(facet_row %||% ".", " ~ ", facet_col %||% ".")
        p <- p + facet_grid(as.formula(fmla))
      }

      # themes & labels
      if (isTRUE(st$theme_enabled) && !is.null(st$theme)) {
        theme_fun <- match.fun(st$theme)
        p <- p + theme_fun(base_size = st$base_size)
      } else {
        p <- p + theme_minimal(base_size = st$base_size %||% 11)
      }
      if (isTRUE(st$labels_enabled)) {
        labs_args <- list()
        if (nzchar(st$labels$title)) labs_args$title <- st$labels$title
        if (nzchar(st$labels$x)) labs_args$x <- st$labels$x
        if (nzchar(st$labels$y)) labs_args$y <- st$labels$y
        p <- p + do.call(ggplot2::labs, labs_args)
      }
      if (isTRUE(st$legend_enabled)) {
        p <- p + theme(legend.position = st$legend_pos)
      } else {
        p <- p + theme(legend.position = "none")
      }

      # color scales
      if (!is.null(col)) {
        # detect if color is numeric
        if (is.numeric(df[[col]])) {
          if (st$colorscale == "Viridis") {
            p <- p + viridis::scale_color_viridis(option = "D")
            if (ptype %in% c("Histogram","Bar")) p <- p + viridis::scale_fill_viridis(option = "D")
          } else if (st$colorscale == "Manual") {
            # manual palette provided as comma-separated hex codes
            pal <- strsplit(st$manual_palette, ",")[[1]]
            pal <- trimws(pal)
            if (length(pal) > 0) p <- p + scale_color_manual(values = pal)
          }
        } else {
          # discrete
          if (st$colorscale == "Viridis") {
            p <- p + viridis::scale_color_viridis_d()
            if (ptype %in% c("Histogram","Bar")) p <- p + viridis::scale_fill_viridis_d()
          } else if (st$colorscale == "Manual") {
            pal <- strsplit(st$manual_palette, ",")[[1]]
            pal <- trimws(pal)
            if (length(pal) > 0) p <- p + scale_color_manual(values = pal)
            if (ptype %in% c("Histogram","Bar")) p <- p + scale_fill_manual(values = pal)
          }
        }
      } else {
        # no color mapping, maybe user wants default manual color
        if (st$colorscale == "Manual") {
          pal <- strsplit(st$manual_palette, ",")[[1]]
          pal <- trimws(pal)
          if (length(pal) >= 1) p <- p + scale_color_manual(values = pal[1])
        }
      }

      p
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
