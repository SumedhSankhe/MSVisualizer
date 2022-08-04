univariatePlotPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box( title = "Univariate Plot", width = 4, status = "warning",
           tabBox(id = "univariatePlot", height = "100%", width = "100%",
                  tabPanel("univariatePlot",
                           selectInput(inputId = ns('sample_group'),
                                       label = 'Categorize by: ',
                                       choices = c('Condition', 'Disease')
                           ),
                           selectInput(inputId = ns("marker_selector"),
                                       choices = NULL, label = NULL)
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                           uiOutput(ns("colorChooser")),
                           numericInput(inputId = ns('x_axis_text_angle'), label = 'x-axis text angle: ', value='0'),
                           numericInput(inputId = ns('plot_width'), label = 'Plot Width: ', value='15'),
                           numericInput(inputId = ns('plot_height'), label = 'Plot Height: ', value='10'),
                           selectInput(inputId = ns('plot_type'),
                                       label = 'Plot Type: ',
                                       choices = c('svg', 'png', 'pdf')
                           )))
      ),
      box(
        status = "warning", width = 8,
        tabBox(height = '100%', width = '100%',
               (div(
                 style='width:1000px;overflow-x: scroll;height:1000px;overflow-y: scroll;',
                 shinycssloaders::withSpinner(
                   plotly::plotlyOutput(ns("univariatePlot")))
               ))
        ),
        downloadButton(ns('downloadPlot'), "Download Plot")
      )
    )
  )
}

univariatePlotPanel <- function(input, output, session, getData) {
  ns <- session$ns

  observe({
    marker_choices <- getData()[[1]][,1]
    updateSelectInput(inputId = 'marker_selector', label = 'Select Marker:',
                      choices = marker_choices)
  })

  output$colorChooser <- renderUI({
    features <- sort(unique(unlist(getData()[[2]][,input$sample_group])))
    lapply(1:length(features), function(x){
      list(colourInput(paste("univariatePanel-col",x, sep="_"), features[x], "purple"))
    })
  })

  colors <- reactive({
    features <- sort(unique(unlist(getData()[[2]][, input$sample_group ])))
    LL <- lapply(seq_along(features), function(i) {
      input[[paste("col", i, sep="_")]]
    })
    if (is.null(input$col_1) ) {
      features <- sort(unique(unlist(getData()[[2]][,input$sample_group])))
      fill_colors <- rep("purple", length(features))
    } else {
      fill_colors <- unlist(LL)
    }
    return(fill_colors)
  })


  map_marker_category <- reactive({
    marker_stats <- data.frame(getData()[[1]])
    marker_stats <- subset(marker_stats, marker_stats[,1]==input$marker_selector)
    marker_stats <- data.frame(names(marker_stats[-1]), t(marker_stats[-1]))

    names(marker_stats) <- c("Sample","Exprs")
    sample_mapping_df <- data.frame(getData()[[2]][c("Sample", input$sample_group)])
    #sample_mapping_df$Sample <- gsub("-", ".", sample_mapping_df$Sample)
    #sample_mapping_df[,input$sample_group] <- gsub("-", ".", sample_mapping_df[,input$sample_group])
    return(merge(marker_stats, sample_mapping_df, by="Sample"))
  })

  output$univariatePlot <- renderPlotly({
    req(input$sample_group)
    req(input$marker)
    ggplotly(
      ggplot(data = map_marker_category(),
             aes(x = get(input$sample_group), y=  Exprs , fill = get(input$sample_group)))+
        scale_fill_manual(values = colors())+
        geom_boxplot(outlier.color = 'red')+
        labs(x = input$sample_group, fill = input$sample_group)+
        theme_bw()+
        theme(text = element_text(size = 16),
              axis.text.x = element_text(angle = input$x_axis_text_angle),
              legend.position = 'top'),

      width = session$clientData$output_pid_width,
      height = session$clientData$output_pid_width
    )
  })


  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("gen_univariateplot", input$plot_type, sep=".")
    },
    content = function(file) {
      switch (input$plot_type,
              svg = {
                svg(file, width = input$plot_width, height = input$plot_height)
              },
              png = {
                png(file, width = input$plot_width, height = input$plot_height)
              },
              pdf = {
                pdf(file, width = input$plot_width, height = input$plot_height)
              }
      )
      plot = gen_univariateplot()
      print(plot)
      dev.off()
    }
  )

}
