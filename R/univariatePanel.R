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
  })

  # output$colorChooser <- renderUI({
  #   features <- sort(unique(unlist(getData()[[2]][,get(input$sample_group)])))
  #   LL <- vector("list", length(features))
  #   for(i in 1:length(features)) {
  #     LL[[i]] <- list(colourInput(paste("univariatePanel-col",i, sep="_"), features[i], "purple"))
  #   }
  #   return(LL)
  # })
  #
  #
  colors <- reactive({
    features <- sort(unique(unlist(getData()[[2]][, get(input$sample_group)])))
    LL <- lapply(seq_along(features), function(i) {
      input[[paste("col", i, sep="_")]]
    })
    if (is.null(input$col_1) ) {
      features <- sort(unique(unlist(getData()[[2]][,get(input$sample_group)])))
      fill_colors <- rep("purple", length(features))
    } else {
      fill_colors <- unlist(LL)
    }
    return(fill_colors)
  })

  toListen <- reactive({
    list(input$marker_selector, input$sample_group, input$x_axis_text_angle)
  })

  observeEvent(toListen(),{
    abundance <- getData()[[1]]
    marker_long <- melt(abundance[UniProtID == input$marker_selector],
                        id.vars = 'UniProtID')
    setnames(marker_long, c('variable', 'value'), c('Sample', 'Exprs'))
    marker_stats <- marker_long[getData()[[2]], on='Sample']

    rv$plot <- ggplot(data = marker_stats,
                      aes(x = get(input$sample_group), y = Exprs ,
                          fill = get(input$sample_group)))+
      geom_boxplot(outlier.color = 'red')+
      labs(x = input$sample_group, fill = input$sample_group)+
      theme_bw()+
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = input$x_axis_text_angle),
            legend.position = 'top')
  })

  output$univariatePlot <- renderPlotly({
    req(!is.null(rv$plot))
    #browser()
    if(length(unique(colors())) == 1){
      ggplotly(rv$plot)
    } else {
      rv$plot <- rv$plot+
        scale_fill_manual(values = )
      ggplotly(rv$plot)
    }
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
