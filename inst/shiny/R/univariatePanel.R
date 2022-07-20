univariatePlotPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("univariatePlot"),
    fluidRow(
      box( title = "Univariate Plot", width = 4, status = "warning",
           tabBox(id = "univariatePlot", height = "100%", width = "100%",
                  tabPanel("univariatePlot",
                           selectInput(inputId = ns('sample_group'),
                                       label = 'Categorize by: ',
                                       choices = c('Condition', 'Disease')
                           ),
                           uiOutput(ns("marker_selector")) #,
                           # actionButton(ns("univariatePlotButton"),
                           #              label = "Submit", 
                           #              icon("paper-plane"), 
                           #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           # )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                           uiOutput(ns("colorChooser")),
                           numericInput(inputId = ns('x_axis_text_angle'), label = 'x-axis text angle: ', value='0'),
                           numericInput(inputId = ns('plot_width'), label = 'Plot Width: ', value='15'),
                           numericInput(inputId = ns('plot_height'), label = 'Plot Height: ', value='10'),
                           selectInput(inputId = ns('plot_type'),
                                       label = 'Plot Type: ',
                                       choices = c('svg', 'png', 'pdf')
                           )                           
                  )                  
           )
      ),
      box(
        status = "warning", width = 8,
        shinycssloaders::withSpinner(
          (div(style='width:800px;overflow-x: scroll;height:800px;overflow-y: scroll;',
               girafeOutput(ns("univariatePlot"), width = "100%", height = "600px")))
        ),
        HTML("<br>"),
        downloadButton(ns('downloadPlot'), "Download Plot")
      )
    )
  )
}

univariatePlotPanel <- function(input, output, session, getData) {
  ns <- session$ns
  
  get_marker_choices <- reactive({
    data <- getData()[[1]]
    marker_choices <- data[,1]
    return(marker_choices)
  })
  
  output$colorChooser <- renderUI({
    features <- sort(unique(unlist(getData()[[2]][,input$sample_group])))
    LL <- vector("list", length(features))
    for(i in 1:length(features)) {
        LL[[i]] <- list(colourInput(paste("univariatePanel-col",i, sep="_"), features[i], "purple"))
    }
    return(LL)
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
    
  output$marker_selector <- renderUI({
    selectInput(inputId = ns('marker'),
                label = 'Select Marker: ',
                choices = get_marker_choices() 
    )  
  })
  
  #map_marker_category <- eventReactive(  input$univariatePlotButton, {
  map_marker_category <- reactive({
    marker_stats <- data.frame(getData()[[1]])
    marker_stats <- subset(marker_stats, marker_stats[,1]==input$marker)
    marker_stats <- data.frame(names(marker_stats[-1]), t(marker_stats[-1]))

    names(marker_stats) <- c("Sample","Exprs")
    sample_mapping_df <- data.frame(getData()[[2]][c("Sample", input$sample_group)])
    #sample_mapping_df$Sample <- gsub("-", ".", sample_mapping_df$Sample)
    #sample_mapping_df[,input$sample_group] <- gsub("-", ".", sample_mapping_df[,input$sample_group])
    return(merge(marker_stats, sample_mapping_df, by="Sample"))
  })
  
  gen_univariateplot <- reactive({

    df <- map_marker_category()
    fill_colors <- colors()
    p <- ggplot(df, aes_string(x=input$sample_group, y="Exprs", fill=input$sample_group)) +
      scale_fill_manual(values = fill_colors) +
      geom_boxplot_interactive(outlier.colour = "red")
    p <- p + theme_minimal() + theme( text = element_text(size=16), axis.text.x = element_text(angle = input$x_axis_text_angle, hjust = 1) )
    p
  })
  
  output$univariatePlot <- renderGirafe({
    # if( is.null(input$sample_group) || is.null(input$marker) ) {
    #   return(NULL)
    # }
    req(input$sample_group)
    req(input$marker)
    p <- gen_univariateplot()
    girafe(ggobj = p,   options = list(opts_toolbar(saveaspng = FALSE)))
  })
  
  # output$downloadPlot <- downloadHandler(
  #   filename = "Univariate_plot.svg",
  #   content = function(file) {
  #     ggsave(file, plot = gen_univariateplot(), device = "svg")
  #   }
  # )
  
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