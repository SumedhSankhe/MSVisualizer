upsetPlotPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("upsetPlot"),
    fluidRow(
      box( title = "Upset Plot", width = 4, status = "warning",
           tabBox(id = "upsetPlot", height = "100%", width = "100%",
                  tabPanel("upsetPlot",
                           selectInput(inputId = ns('upset_by'),
                                       label = 'Upset by: ',
                                       choices = c('Condition', 'Disease', 'Sample')
                           ),
                           actionButton(ns("upsetPlotButton"), 
                                        label = "Submit", 
                                        icon("paper-plane"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                           #checkboxInput(inputId=ns('empty_intersections'), label="Show Empty Sets: ", value = FALSE, width = NULL),
                           #selectInput(inputId = ns('mode_type'),
                           #             label = 'Mode: ',
                           #             choices = c('exclusive_intersection', 'inclusive_intersection', 'exclusive_union', 'inclusive_union')),
                           numericInput(inputId = ns('min_size'), label = 'Min. Set Size: ', value='5'),
                           numericInput(inputId = ns('label_size'), label = 'Label Size: ', value='15'),
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
        tabBox(id = "displayTab", height = "100%", width = "100%",
          tabPanel(title="Upset Plot",
            shinycssloaders::withSpinner(
              (div(style='width:800px;overflow-x: scroll;height:800px;overflow-y: scroll;',
              plotOutput(ns("upsetPlot"), width = "150%", height = "600px")))
            ),
            HTML("<br>"),
            downloadButton(ns('downloadUpsetplot'), "Download Plot")
          ),
          tabPanel("Upset Results",
            (div(style='width:800px;overflow-x: scroll;height:800px;overflow-y: scroll;',
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
  
  get_sample_upset_matrix <- function(matrix_stats) {
    row.names(matrix_stats) <- matrix_stats[,1] #matrix_stats$UniProtID
    matrix_stats <- matrix_stats[,2:ncol(matrix_stats)]
    upset_by_sample <- floor(matrix_stats)
    upset_by_sample[upset_by_sample > 0] <- 1
    return(upset_by_sample)
  }
  
  compute_upset_by_sample <- reactive({
    matrix_stats <- data.frame(getData()[[1]])
    return(get_sample_upset_matrix(matrix_stats))
  })
  
  compute_upset_by_disease <- reactive({
    sample_category_df <- data.frame(getData()[[2]])
    sample_category_df$Sample <- gsub("-", ".", sample_category_df$Sample)
    diseases <- unique(sample_category_df$Disease)
    matrix_stats <- data.frame(getData()[[1]])
    upset_by_sample <- get_sample_upset_matrix(matrix_stats)
    upset_by_disease <- data.frame( matrix(ncol=length(diseases), nrow = nrow(upset_by_sample)) )
    colnames(upset_by_disease) <- diseases
    rownames(upset_by_disease) <- rownames(upset_by_sample)
    for (disease in diseases ) {
      col_names <- sample_category_df[sample_category_df$Disease == disease, "Sample"]
      if( length(col_names) > 1) {
        upset_by_disease[disease] <- rowSums(upset_by_sample[, col_names])
      } else {
        upset_by_disease[disease] <- upset_by_sample[, col_names]
      }
    }
    upset_by_disease[upset_by_disease > 0] <- 1
    return(upset_by_disease)
  })

  compute_upset_by_condition <- reactive({
    sample_category_df <- data.frame(getData()[[2]])
    sample_category_df$Sample <- gsub("-", ".", sample_category_df$Sample)
    conditions <- unique(sample_category_df$Condition)
    matrix_stats <- data.frame(getData()[[1]])
    upset_by_sample <- get_sample_upset_matrix(matrix_stats)
    upset_by_condition <- data.frame( matrix(ncol=length(conditions), nrow = nrow(upset_by_sample)) )
    colnames(upset_by_condition) <- conditions
    rownames(upset_by_condition) <- rownames(upset_by_sample)
    for (condition in conditions ) {
      col_names <- sample_category_df[sample_category_df$Condition == condition, "Sample"]
      upset_by_condition[condition] <- rowSums(upset_by_sample[, col_names])
    }
    upset_by_condition[upset_by_condition > 0] <- 1
    return(upset_by_condition)
  })
  
    
  compute_upset_matrix <- eventReactive(  input$upsetPlotButton, {
    set.seed(100)
    switch (input$upset_by,
            Sample = {
              upset_matrix <- compute_upset_by_sample()
            },
            Condition = {
              upset_matrix <- compute_upset_by_condition()
            },
            Disease = {
              upset_matrix <- compute_upset_by_disease()
            }
    )
    row_names <- row.names(upset_matrix)
    upset_matrix <- data.frame(sapply(upset_matrix, as.logical))
    colnames(upset_matrix) <- make.names(names(upset_matrix))
    rownames(upset_matrix) <- row_names
    return( upset_matrix )
  })
  
  gen_upsetplot <- reactive({
    df <- compute_upset_matrix()
    p <- upset(df, colnames(df), min_size=input$min_size) + #, mode = input$mode_type) + 
      theme(text=element_text(size=input$label_size))

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

    return(p)    
  })
  
  output$upsetPlot <- renderPlot({
    p <- gen_upsetplot()
    p
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
      plot = gen_upsetplot()
      print(plot)
      dev.off()
    }
  )
  
  output$upsetResults <- DT::renderDataTable({
    req(compute_upset_matrix())
    datatable(compute_upset_matrix(), filter = "top")
  }, server = FALSE)
  
  output$filtered_row <- renderPrint({
      input[["upsetResults_rows_all"]]
    })
  
  output$downloadUpsetReport <- downloadHandler(
    filename = function() {
      "Upset_Results.csv"
    },
    content = function(file) {
      upset_report <- as.data.frame(compute_upset_matrix())
      write.csv(upset_report[input[["upsetResults_rows_all"]],], file, row.names=TRUE, quote=FALSE)
    }
  )
}