cosinePanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Cosine Similarity/Dissimilarity plots"),
    fluidRow(
      box( title = "Control Box", width = 4, status = "warning",
           tabBox(id = "cosinePlot", height = "100%", width = "100%",
                  tabPanel("cosinePlot",
                           selectInput(inputId = ns('column_col'),
                                       label = 'Color Samples By: ',
                                       choices = c('Disease', 'Condition')
                           ),
                           selectInput(inputId = ns('dissim'),
                                       label = 'Measure',
                                       choice = c('Similarity', 'Dissimilarity')
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
  rv <- reactiveValues()
  observeEvent(input$cosinePlotButton,{
     abundance <- getData()[[1]]
     annotations <- getData()[[2]]

     abundance_mat <- as.matrix(abundance[,-'UniProtID'])
     rv$similarity <- calc_cosine_similarity(matrix = t(abundance_mat))

     if(input$dissim != 'Similarity'){
       rv$similarity <- as.dist(1-rv$similarity)
     }

     rv$sample_col <- create_annotation_col(ann = annotations,
                                            col = isolate(input$column_col))

     output$cosinePlot <- renderPlot({
       pheatmap::pheatmap(rv$similarity, annotation_col = rv$sample_col)
     })
  })


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
      pheatmap::pheatmap(rv$similarity, annotation_col = rv$sample_col)
      dev.off()
    }
  )

}

#https://stats.stackexchange.com/questions/31565/compute-a-cosine-dissimilarity-matrix-in-r
calc_cosine_similarity <- function(matrix){
  sim <- matrix/sqrt(rowSums(matrix*matrix))
  sim%*%t(sim)
}



