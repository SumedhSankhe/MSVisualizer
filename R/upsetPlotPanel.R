upsetPlotPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box( title = "Control Box", width = 4, status = "warning",
           tabBox(id = "upsetPlot", height = "100%", width = "100%",
                  tabPanel("Upset Plot",
                           selectInput(inputId = ns('upset_by'),
                                       label = 'Upset by: ',
                                       choices = c('Condition', 'Disease', 'Sample')
                           ),
                           numericInput(inputId = ns('min_size'),
                                        label = 'Min. Set Size: ', value='5'),
                           actionButton(ns("upsetPlotButton"),
                                        label = "Submit",
                                        icon("paper-plane"),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Download Settings"),
                           #checkboxInput(inputId=ns('empty_intersections'),
                           #label="Show Empty Sets: ", value = FALSE, width = NULL),
                           #selectInput(inputId = ns('mode_type'),
                           #             label = 'Mode: ',
                           #             choices = c('exclusive_intersection',
                           #'inclusive_intersection', 'exclusive_union', 'inclusive_union')),
                           numericInput(inputId = ns('label_size'),
                                        label = 'Label Size: ', value='15'),
                           numericInput(inputId = ns('plot_width'),
                                        label = 'Plot Width: ', value='15'),
                           numericInput(inputId = ns('plot_height'),
                                        label = 'Plot Height: ', value='10'),
                           selectInput(inputId = ns('plot_type'),
                                       label = 'Plot Type: ',
                                       choices = c('svg', 'png', 'pdf')
                           )
                  )
           )
      ),
      box(
        status = "warning", width = 8,
        tabBox(id = "displayTab", height = "100%", width = "100%",
               tabPanel(title="Upset Plot",
                        shinycssloaders::withSpinner(
                          (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                               plotOutput(ns("upsetPlot"))))
                        ),
                        HTML("<br>"),
                        downloadButton(ns('downloadUpsetplot'), "Download Plot")
               ),
               tabPanel("Upset Results",
                        (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                             dataTableOutput(ns("upsetResults")))),
                        HTML("<br>"),
                        downloadButton(ns("downloadUpsetReport"), "Download csv")
               )
        )
      )
    )
  )
}

upsetPlotPanel <- function(input, output, session, getData) {
  ns <- session$ns
  rv <- reactiveValues()
  observeEvent(input$upsetPlotButton,{
    switch (input$upset_by,
            Sample = {
              rv$upset_matrix <- get_upset_by_sample(matrix_stats = getData()[[1]])
            },
            Condition = {
              rv$upset_matrix <- get_upset_by_group(dt = getData()[[2]],
                                                 dt2 = getData()[[1]],
                                                 group = 'Condition')
            },
            Disease = {
              rv$upset_matrix <- get_upset_by_group(dt = getData()[[2]],
                                                 dt2 = getData()[[1]],
                                                 group = 'Disease')
            }
    )
    rn <- rv$upset_matrix[,(UniProtID)]
    rv$upset_matrix <- rapply(rv$upset_matrix[,-'UniProtID'], as.logical,
                           classes = 'numeric', how = 'replace')
    rownames(rv$upset_matrix) <- rn
    colnames(rv$upset_matrix) <- make.names(names(rv$upset_matrix))

    rv$plot <- ComplexUpset::upset(data = rv$upset_matrix,
                                   intersect = colnames(rv$upset_matrix),
                                   #mode = input$mode_type,
                                   min_size = input$min_size,
                                   name = input$upset_by)+
      theme(text = element_text(size = input$label_size))
    ##########
    # if (input$empty_intersections) {
    #   # p <-  upset(data.frame(sapply(df, as.numeric)),
    #   #             nintersects = NA,
    #   #             order.by = "freq",
    #   #             nsets = ncol(df),
    #   #             decreasing = T,
    #   #             mb.ratio = c(0.6, 0.4),
    #   #             number.angles = 0,
    #   #             text.scale = 1.8,
    #   #             point.size = 2.8,
    #   #             line.size = 1,
    #   #             empty.intersections = "true")
    # } else {
    #   p <-  upset(data.frame(sapply(df, as.numeric)),
    #               nintersects = NA,
    #               order.by = "freq",
    #               nsets = ncol(df),
    #               decreasing = T,
    #               mb.ratio = c(0.6, 0.4),
    #               number.angles = 0,
    #               text.scale = 1.8,
    #               point.size = 2.8,
    #               line.size = 1)
    # }
    ########

    output$upsetPlot <- renderPlot({
      rv$plot
    })

    output$upsetResults <- DT::renderDataTable({
      req(rv$upset_matrix)
      DT::datatable(rv$upset_matrix, filter = "top")
    }, server = FALSE)

    output$filtered_row <- renderPrint({
      input[["upsetResults_rows_all"]]
    })

  })

  output$downloadUpsetplot <- downloadHandler(
    filename = function() {
      paste("upset_plot", input$plot_type, sep=".")
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



  output$downloadUpsetReport <- downloadHandler(
    filename = function() {
      "Upset_Results.csv"
    },
    content = function(file) {
      upset_report <- as.data.frame(rv$upset_matrix)
      write.csv(upset_report[input[["upsetResults_rows_all"]],], file, row.names=TRUE, quote=FALSE)
    }
  )
}
