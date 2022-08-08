dimReductionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Dimensionality Reduction"),
    fluidRow(
      box( title = "Control Box", width = 4, status = "primary",
           tabBox(id = "dimred", height = "100%", width = "100%",
                  tabPanel("Projection",
                           uiOutput(ns("dimred_markers_selector")),
                           uiOutput(ns("cmarker_selector")),
                           selectInput(inputId = ns('drmethod'),
                                       label = 'Projection Method: ',
                                       choices = c('PCA', 'tSNE', 'UMAP')
                           ),
                           numericInput(inputId = ns('x_dim'),
                                        label = 'X-axis Dimension: ', value='1'),
                           numericInput(inputId = ns('y_dim'),
                                        label = 'Y-axis Dimension: ', value='2'),

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
               tabPanel(title="Projections",
                        (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                        shinycssloaders::withSpinner(
                          plotlyOutput(ns("projectedPlot")))
                          )
                        ),
                        HTML("<br>"),
                        downloadButton(ns('downloadProjPlot'), "Download Plot")
               ),

               tabPanel(title="Contributions",
                        (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                        shinycssloaders::withSpinner(
                          plotOutput(ns("eigen_contrib_plot")))
                          )
                        ),
                        br(),
                        shinycssloaders::withSpinner(
                          plotOutput(ns("var_contrib_plot")
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
  #ranges <- reactiveValues(x = NULL, y = NULL)
  rv <- reactiveValues()

  observe({
    abundance <- as.matrix(getData()[[1]], rownames = T)
    rv$abundance <- as.data.frame(t(abundance))
    rv$annotation <- getData()[[2]]
    rv$annotation[,':='(Sample = gsub('-','.', Sample),
                       Condition = gsub('-','.', Condition))]

    isNumeric <- vapply(rv$abundance, is.numeric, logical(1))
    rv$numeric.cols <- names(isNumeric)[isNumeric]
    rv$feature_choices <- names(rv$abundance)
  })

  output$selectNumericCols <- renderUI({
    selectInput(ns("numericCol"), "Columns with Numeric Values",
                choices = rv$numeric.cols, multiple=TRUE, selected = NULL)
  })

  output$dimred_markers_selector <- renderUI({
    selectizeInput(inputId = ns('smarkers'), label = 'Select Features: ',
                   multiple=TRUE, choices = rv$numeric.cols,
                   options = list(maxItems = 100, placeholder = 'All')
    )
  })

  output$cmarker_selector <- renderUI({
    selectInput(inputId = ns('cmarker'), label = 'Color By Feature: ',
                choices = get_classification_choices()
    )
  })

  tsne_projs <- callModule(tsnePanel, "tsne", rv$abundance, input$smarkers,
                           rv$annotation, proj_dims)
  pca_projs  <- callModule(pcaPanel, "pca", rv$abundance, input$smarkers,
                           rv$annotation)
  umap_projs <- callModule(umapPanel, "umap", rv$abundance, input$smarkers,
                           rv$annotation, proj_dims)


  proj_dims <- reactive({
    return(c(input$x_dim, input$y_dim))
  })

  get_classification_choices <- reactive({
    return( names(getData()[[2]])[-1] )
  })


  observeEvent( input$dimredButton, {
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

    plotdt <-proj_results$projOutput
    rv$plot <- ggplot(data = plotdt,
                      aes_string(x = names(plotdt)[2], y = names(plotdt)[3],
                                 group = input$cmarker, color = input$cmarker))+
      geom_point()+
      stat_ellipse()+
      theme_bw()+
      theme(text = element_text(size = 16))

    output$projectedPlot <- renderPlotly({
      ggplotly(rv$plot)
    })

    if(input$drmethod == 'PCA'){
      output$eigen_contrib_plot <- renderPlot({
        factoextra::fviz_eig(proj_results$pca, addlabels = TRUE,
                             ylim = c(0, 50))

      })

      output$var_contrib_plot <- renderPlot({
        factoextra::fviz_pca_var(proj_results$pca,
                            axes = c(isolate(input$x_dim), isolate(input$y_dim)),
                            col.var = "contrib",
                            select.var=list(contrib=5),
                            repel=TRUE)

      })

      output$pca_contrib_results <- DT::renderDataTable({
        rv$contrib <- as.data.frame(
          factoextra::get_pca_var(proj_results$pca)$contrib
        )
        rv$contrib
      })
    }
  })

  # get_projectedplot <- reactive({
  #   req(compute_projs())
  #   #req(selected_markers())
  #
  #   if (isolate(input$drmethod) == 'PCA') {
  #     color_marker <- compute_projs()$projInput %>% dplyr::select( isolate(input$cmarker) ) %>% unlist()
  #     projected_ouput <- compute_projs()$projOutput
  #     projected_ouput$tooltip <- rownames(projected_ouput)
  #     p <- fviz_pca_ind(projected_ouput,
  #                               axes = c(isolate(input$x_dim), isolate(input$y_dim)),
  #                               addEllipses = TRUE,
  #                               geom.ind = "point",
  #                               geom.text=color_marker,
  #                               pointshape=21,
  #                               pointsize = 2.5,
  #                               fill.ind = color_marker,
  #                               repel=TRUE) + scale_color_viridis_d()
  #   }
  #   else {
  #     plot_data <- cbind(compute_projs()$projOutput, compute_projs()$projInput %>% dplyr::select( isolate(input$cmarker) ))
  #
  #     if (class(plot_data[[ isolate(input$cmarker) ]]) %in% c("numeric","integer") ) {
  #       p <- ggplot(plot_data, aes_string(x = names(plot_data)[1], y = names(plot_data)[2]), group= isolate(input$cmarker) ) +
  #           geom_point(aes_string(color=input$cmarker), size=3) + scale_color_viridis_c()
  #     } else {
  #       p <- ggplot(plot_data, aes_string(x = names(plot_data)[1], y = names(plot_data)[2]), group= isolate(input$cmarker) ) +
  #           geom_point(aes_string(color= isolate(input$cmarker) ), size=3) + scale_colour_viridis_d()
  #     }
  #     p <- p + theme( text = element_text(size=16) )
  #   }
  #   p
  # })

  # output$projectedPlot <- renderPlotly({
  #   p <- ggplotly(get_projectedplot(), tooltip = "text")
  #   for (i in 1:length(p$x$data)){
  #     if (!is.null(p$x$data[[i]]$name)){
  #       p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
  #     }
  #   }
  #   p
  # })





  output$download_pca_contrib <- downloadHandler(
    filename = function() {
      "PCA_Contributions.csv"
    },
    content = function(file) {
      if (isolate(input$drmethod) == 'PCA') {
        write.csv(rv$contrib, file, row.names=TRUE, quote=FALSE)
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
      rv$plot
      dev.off()
    }
  )
}
