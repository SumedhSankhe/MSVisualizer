univariatePlotPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Univariate Plot"),
    fluidRow(
      box( title = 'Control Box', width = 4, status = "primary",
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
                           actionButton(inputId = ns('update_colors'),
                                        label = 'Update Color Scheme'),
                           numericInput(inputId = ns('x_axis_text_angle'),
                                        label = 'x-axis text angle: ', value='0'),
                           numericInput(inputId = ns('plot_width'),
                                        label = 'Plot Width: ', value='15'),
                           numericInput(inputId = ns('plot_height'),
                                        label = 'Plot Height: ', value='10'),
                           selectInput(inputId = ns('plot_type'),
                                       label = 'Plot Type: ',
                                       choices = c('svg', 'png', 'pdf')
                           )))
      ),
      box(title = 'Visualization', status = "warning", width = 8,
          tabBox(height = '100%', width = '100%',
                 (div(
                   style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
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
  rv <- reactiveValues()

  observe({
    marker_choices <- getData()[[1]][,1]
    updateSelectInput(inputId = 'marker_selector', label = 'Select Marker:',
                      choices = marker_choices)

    rv$features <- unique(getData()[[2]][, get(input$sample_group)])

    rv$colors <- lapply(seq_along(rv$features), function(x){
      colourInput(
        inputId = sprintf("univariatePanel-col_%s",rv$features[x]),
        label = rv$features[x], value = 'black'
      )
    })

    output$colorChooser <- renderUI({
      rv$colors
    })
  })

  new_colors <- reactive({
    unlist(
      sapply(rv$features, function(x){
        input[[paste('col',x, sep = '_')]]
      }, USE.NAMES = T)
    )
  })

  toListen <- reactive({
    list(input$marker_selector, input$sample_group, input$x_axis_text_angle,
         input$update_colors)
  })

  observeEvent(toListen(),{
    abundance <- getData()[[1]]
    marker_long <- melt(abundance[UniProtID == input$marker_selector],
                        id.vars = 'UniProtID')
    setnames(marker_long, c('variable', 'value'), c('Sample', 'Exprs'))
    marker_stats <- marker_long[getData()[[2]], on='Sample']


    if(length(rv$features) != length(new_colors())){
      rv$new_colors <- rep('black', length(rv$features))
    }else{
      rv$new_colors <- new_colors()
    }

    rv$plot <- ggplot(data = marker_stats,
                      aes(x = get(input$sample_group), y = Exprs ,
                          fill = get(input$sample_group)))+
      geom_boxplot(outlier.color = 'red')+
      labs(x = input$sample_group, fill = input$sample_group)+
      theme_bw()+
      scale_fill_manual(values = rv$new_colors)+
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = input$x_axis_text_angle),
            legend.position = 'top')
  })

  output$univariatePlot <- renderPlotly({
    req(!is.null(rv$plot))
    #browser()
    #if(length(unique(colors())) == 1){
    ggplotly(rv$plot)
    #} else {
    # rv$plot <- rv$plot+
    #    scale_fill_manual(values = )
    # ggplotly(rv$plot)
    #}
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
      print(rv$plot)
      dev.off()
    }
  )

}
