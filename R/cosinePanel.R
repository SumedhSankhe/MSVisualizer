cosinePanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("cosinePlot"),
    fluidRow(
      box( title = "Cosine Similarity Plot & Visualization", width = 4, status = "warning",
           tabBox(id = "cosinePlot", height = "100%", width = "100%",
                  tabPanel("cosinePlot",
                           selectInput(inputId = ns('column_col'),
                                       label = 'Color Samples By: ',
                                       choices = c('Disease', 'Condition')
                           ),                           
                           actionButton(ns("cosinePlotButton"), 
                                        label = "Submit", 
                                        icon("paper-plane"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                           numericInput(inputId = ns('plot_width'), label = 'Plot Width: ', value='10'),
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
          (div(style='width:900px;overflow-x: scroll;height:900px;overflow-y: scroll;',
               shinycssloaders::withSpinner(
               plotOutput(ns("cosinePlot"), width = "800px", height = "800px")))
        ),
        HTML("<br>"),
        downloadButton(ns('downloadCosineplot'), "Download Plot")
        
      )
    )
  )
}

cosinePanel <- function(input, output, session, getData) {
  ns <- session$ns
  
  getDataMatrix <- eventReactive( input$cosinePlotButton, {
    df <- data.frame(getData()[[1]])
    row.names(df) <- df[,1]
    cosine_matrix <- cosine(as.matrix(df[,2:ncol(df)]))
    cosine_matrix
  })

  gen_cosineplot <- reactive({
    req(getDataMatrix())
    df <- getDataMatrix()
    sample_category_df <- data.frame(getData()[[2]])
    sample_category_df$Sample <- gsub("-", ".", sample_category_df$Sample)
    sample_col <- data.frame(SampleType=sample_category_df[isolate(input$column_col)])
    row.names(sample_col) <- sample_category_df$Sample
    p <- pheatmap(df, annotation_col = sample_col)
    return(p)
    
  })
    
  output$cosinePlot <- renderPlot({
    req(getDataMatrix())
    p <- gen_cosineplot()
    p
  })
  
  # output$downloadCosineplot <- downloadHandler(
  #   filename = "cosine_plot.svg",
  #   content = function(file) {
  #     ggsave(file, plot = gen_cosineplot(), device = "svg", width = input$plot_width, height = input$plot_height, units="in")
  #   }
  # )
  
  output$downloadCosineplot <- downloadHandler(
    filename = function() {
      paste("cosine_plot", input$plot_type, sep=".")
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
      plot = gen_cosineplot()
      print(plot)
      dev.off()
    }
  )
  
}
