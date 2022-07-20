dimReductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Dimensionality Reduction"),
    fluidRow(
      box( title = "Dimensionality Reduction & Visualization", width = 4, status = "warning",
           tabBox(id = "dimred", height = "100%", width = "100%",
                  tabPanel("Projection",
                           uiOutput(ns("dimred_markers_selector")),
                           uiOutput(ns("cmarker_selector")),
                           selectInput(inputId = ns('drmethod'),
                                       label = 'Projection Method: ',
                                       choices = c('PCA', 'tSNE', 'UMAP')
                           ),
                           numericInput(inputId = ns('x_dim'), label = 'X-axis Dimension: ', value='1'),
                           numericInput(inputId = ns('y_dim'), label = 'Y-axis Dimension: ', value='2'),
                           
                           actionButton(ns("dimredButton"),
                                        label = "Submit",
                                        icon("paper-plane"),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                    conditionalPanel("input.drmethod == 'tSNE'", ns=ns,
                      tsnePanelUI(ns("tsne"))
                    ),
                    conditionalPanel("input.drmethod == 'PCA'", ns=ns,
                      pcaPanelUI(ns("pca"))
                    ),
                    conditionalPanel("input.drmethod == 'UMAP'", ns=ns,
                      umapPanelUI(ns("umap"))
                    ),
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
            tabPanel(title="Projections",
              shinycssloaders::withSpinner(
                plotlyOutput(ns("projectedPlot")
                  # , 
                  # dblclick = ns("plot_dblclick"), 
                  # brush = brushOpts(id = ns("plot_brush"), resetOnNew = TRUE) 
                )
              ),
              HTML("<br>"),
              downloadButton(ns('downloadProjPlot'), "Download Plot")
            ),

            tabPanel(title="Contributions",
               shinycssloaders::withSpinner(
                 plotOutput(ns("eigen_contrib_plot")
                            # , 
                            # dblclick = ns("plot_dblclick"), 
                            # brush = brushOpts(id = ns("plot_brush"), resetOnNew = TRUE) 
                 )
               ),
               br(),
               shinycssloaders::withSpinner(
                 plotOutput(ns("var_contrib_plot")
                            # , 
                            # dblclick = ns("plot_dblclick"), 
                            # brush = brushOpts(id = ns("plot_brush"), resetOnNew = TRUE) 
                 )
               ),
               br(),
               dataTableOutput(ns("pca_contrib_results")),
               HTML("<br>"),
               downloadButton(ns("download_pca_contrib"), "Download csv")
            )
          )
      )
    )
  )
}

dimReduction <- function(input, output, session, getData) {
  ns <- session$ns
  ranges <- reactiveValues(x = NULL, y = NULL)

  getClassificationData <- reactive({
    
    phenoTable <- data.frame(getData()[[2]])
    phenoTable$Sample <- gsub("-", ".", phenoTable$Sample)
    phenoTable$Condition <- gsub("-", ".", phenoTable$Condition)
    phenoTable
  })
  
  getMatrixData <- reactive({
    df <- data.frame(getData()[[1]])
    row.names(df) <- unlist(df[1])
    df <- t(df[-1])
    as.data.frame(df)
  })

  ########################
  ##select numeric columns
  ########################
  getNumericCols <- reactive({
      data <- getMatrixData()
      cols <- colnames(data)
      classes <- sapply(data, class)
      univ <- names(classes[classes == "numeric"])
      cols <- cols[cols %in% univ]
      cols
  })

  output$selectNumericCols <- renderUI({
      cols <- getNumericCols()
      selectInput(ns("numericCol"), "Columns with Numeric Values",
                  choices = cols,
                  multiple=TRUE,
                  selected = NULL)
  })  
  
  get_feature_choices <- reactive({
    return( getMatrixData() %>% names() )
  })
  
  output$dimred_markers_selector <- renderUI({
    selectizeInput(inputId = ns('smarkers'),
                  label = 'Select Features: ', 
                  multiple=TRUE,
                  choices = getNumericCols(),
                  options = list(maxItems = 100, placeholder = 'All')
    )
  })
  
  output$cmarker_selector <- renderUI({
    selectInput(inputId = ns('cmarker'),
               label = 'Color By Feature: ',
               choices = get_classification_choices() #get_feature_choices()
    )  
  })
    
  selected_markers <- reactive({
    return(input$smarkers)
  })
  
  proj_dims <- reactive({
    return(c(input$x_dim, input$y_dim))
  })
  
  get_classification_choices <- reactive({
    return( names(getData()[[2]])[-1] )
  })
  
  # output$classificationChoice <- renderUI({
  #   selectInput(inputId = ns('classificationmethod'),
  #               label = 'Classification: ',
  #               choices = get_classification_choices()
  #   )
  # })
    
  tsne_projs <- callModule(tsnePanel, "tsne", getMatrixData, selected_markers, getClassificationData, proj_dims)
  pca_projs  <- callModule(pcaPanel, "pca", getMatrixData, selected_markers, getClassificationData)
  umap_projs <- callModule(umapPanel, "umap", getMatrixData, selected_markers, getClassificationData, proj_dims)
  
  compute_projs <- eventReactive( input$dimredButton, {
    set.seed(100)
    # # Create a Progress object
    # progress <- shiny::Progress$new()
    # progress$set(message = "Sit Back and Relax", value = 0)
    # # Close the progress when this reactive exits (even if there's an error)
    # on.exit(progress$close())
    switch (input$drmethod,
            tSNE = {
              proj_results <- tsne_projs()
            },
            PCA = {
              proj_results <- pca_projs()
            },
            UMAP = {
              proj_results <- umap_projs()
            }
    )
    return( proj_results )
  })
  
  get_projectedplot <- reactive({
    req(compute_projs())
    #req(selected_markers())
    
    if (isolate(input$drmethod) == 'PCA') {
      color_marker <- compute_projs()$projInput %>% dplyr::select( isolate(input$cmarker) ) %>% unlist()
      projected_ouput <- compute_projs()$projOutput
      projected_ouput$tooltip <- rownames(projected_ouput)
      p <- fviz_pca_ind(projected_ouput,
                                axes = c(isolate(input$x_dim), isolate(input$y_dim)),
                                addEllipses = TRUE, 
                                geom.ind = "point", 
                                geom.text=color_marker, 
                                pointshape=21, 
                                pointsize = 2.5, 
                                fill.ind = color_marker, 
                                repel=TRUE) + scale_color_viridis_d() 
    }
    else {
      plot_data <- cbind(compute_projs()$projOutput, compute_projs()$projInput %>% dplyr::select( isolate(input$cmarker) ))
      
      if (class(plot_data[[ isolate(input$cmarker) ]]) %in% c("numeric","integer") ) {
        p <- ggplot(plot_data, aes_string(x = names(plot_data)[1], y = names(plot_data)[2]), group= isolate(input$cmarker) ) +
            geom_point(aes_string(color=input$cmarker), size=3) + scale_color_viridis_c() 
      } else {
        p <- ggplot(plot_data, aes_string(x = names(plot_data)[1], y = names(plot_data)[2]), group= isolate(input$cmarker) ) +
            geom_point(aes_string(color= isolate(input$cmarker) ), size=3) + scale_colour_viridis_d()
      }
      p <- p + theme( text = element_text(size=16) )
    }
    p
  })
  
  output$projectedPlot <- renderPlotly({
    p <- ggplotly(get_projectedplot(), tooltip = "text")
    for (i in 1:length(p$x$data)){
      if (!is.null(p$x$data[[i]]$name)){
        p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
      }
    }
    p
  })
  
  output$eigen_contrib_plot <- renderPlot({
    req(compute_projs())
    #req(selected_markers())
    
    if (isolate(input$drmethod) == 'PCA') {
      p <- fviz_eig(compute_projs()$projOutput, addlabels = TRUE, ylim = c(0, 50))
    }
    else {
      p <- ggplot()
    }
    p
    
  })

  output$var_contrib_plot <- renderPlot({
    req(compute_projs())
    #req(selected_markers())
    
    if (isolate(input$drmethod) == 'PCA') {
      p <- fviz_pca_var(compute_projs()$projOutput, 
                        axes = c(isolate(input$x_dim), isolate(input$y_dim)),
                        col.var = "contrib", 
                        select.var=list(contrib=5), 
                        repel=TRUE)
    }
    else {
      p <- ggplot()
    }
    p
    
  })
  
  output$pca_contrib_results <- DT::renderDataTable({
    req(compute_projs())
    if (isolate(input$drmethod) == 'PCA') {
      as.data.frame(get_pca_var(compute_projs()$projOutput)$contrib)
    }
    else {
      data.frame()
    }
  })
  
  output$download_pca_contrib <- downloadHandler(
    filename = function() {
      "PCA_Contributions.csv"
    },
    content = function(file) {
      if (isolate(input$drmethod) == 'PCA') {
        pca_contrib_resutls <- as.data.frame(get_pca_var(compute_projs()$projOutput)$contrib)
        write.csv(deseq_contrib_resutls, file, row.names=TRUE, quote=FALSE)
      }
    }
  )
  
  output$downloadProjPlot <- downloadHandler(
    filename = function() {
      paste("projected_plot", input$plot_type, sep=".")
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
      plot = get_projectedplot()
      print(plot)
      dev.off()
    }
  )
  
  # output$downloadProjPlot <- downloadHandler(
  #   filename = "projected_plot.svg",
  #   contentType = "image/svg",
  #   content = function(file) {
  #     svg(file)#, width = input$plot_width, height = input$plot_height)
  #     plot = get_projectedplot()
  #     print(plot)
  #     dev.off()
  #   }
  # )
  
  # # When a double-click happens, check if there's a brush on the plot.
  # # If so, zoom to the brush bounds; if not, reset the zoom.
  # observeEvent(input$plot_dblclick, {
  #   brush <- input$plot_brush
  #   if (!is.null(brush)) {
  #     ranges$x <- c(brush$xmin, brush$xmax)
  #     ranges$y <- c(brush$ymin, brush$ymax)
  # 
  #   } else {
  #     ranges$x <- NULL
  #     ranges$y <- NULL
  #   }
  # })
}
