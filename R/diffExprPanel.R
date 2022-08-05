diffExprPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Differential Expression Plots"),
    fluidRow(
      box( title = "Control Box", width = 4, status = "primary",
           tabBox(id = "diffexpr", height = "100%", width = "100%",
                  tabPanel("Volcano Plot",
                           numericInput(inputId = ns('pCutoff'),
                                        label = 'P-val Cutoff: ', value='1e-5'),
                           #numericInput(inputId = ns('AdjpCutoff'), label = '-Log10_Adj P-val Cutoff: ', value='2'),
                           numericInput(inputId = ns('FCcutoff'),
                                        label = 'Log2FC Cutoff: ', value='2.0'),
                           selectInput(inputId = ns('selectCondition1'),
                                       choices = NULL,
                                       label = 'Condition 1'),
                           selectInput(inputId = ns('selectCondition2'),
                                       choices = NULL,
                                       label = 'Condition 2'),
                           conditionalPanel(
                             condition = "input.selectCondition1 == 'CUSTOM'",
                             selectizeInput(inputId = ns("selectCondition1Cols"),
                                            choices = NULL, multiple = TRUE,
                                            label = 'Condition1 Samples')
                             ),
                           conditionalPanel(
                             condition = "input.selectCondition2 == 'CUSTOM'",
                             selectizeInput(inputId = ns("selectCondition2Cols"),
                                            choices = NULL, multiple = TRUE,
                                            label = 'Condition2 Samples')
                           ),
                           #uiOutput(ns("classificationChoice")),
                           actionButton(ns("volcanoPlotButton"),
                                        label = "Submit",
                                        icon("paper-plane"),
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Download Settings"),
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
      box(title = 'Visualization', status = "warning", width = 8,
          tabBox(id = "displayTab", height = "100%", width = "100%",
                 tabPanel(title="Volcano Plot",
                          (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                               shinycssloaders::withSpinner(
                                 plotOutput(ns("volcanoPlot")))
                          )),
                          HTML("<br>"),
                          downloadButton(ns('downloadVolcanoPlot'), "Download Plot")
                 ),
                 tabPanel("Diff Expr Results",
                          (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                               dataTableOutput(ns("diffExprResults")))),
                          HTML("<br>"),
                          downloadButton(ns("downloadDiffExprReport"), "Download csv")
                 )
          )
      )
    )
  )
}

diffExprPanel <- function(input, output, session, getData) {
  ns <- session$ns
  rv <- reactiveValues()

  #populate the samples select inputs
  observe({
    req(!is.null(getData()))
    groups <- c(unique(getData()[[2]][,(Condition)]),
                unique(getData()[[2]][,(Disease)]), 'CUSTOM')

    lapply(c('selectCondition1','selectCondition2'), function(x){
      updateSelectInput(inputId = x, choices = groups)
    })
  })

  toListen <- reactive({
    list(input$selectCondition1, input$selectCondition2)
  })

  observeEvent(toListen(),{
    isNumeric <- vapply(getData()[[1]], is.numeric, logical(1))
    numeric.cols <- names(isNumeric)[isNumeric]
    lapply(c("selectCondition1Cols","selectCondition2Cols"), function(x){
      updateSelectizeInput(session = session, inputId = x, choices = numeric.cols,
                           server = T, selected = NULL)
    })
  })


  observeEvent(input$volcanoPlotButton,{

    abundance <- getData()[[1]]
    annotation <- getData()[[2]]
    annot_long <- melt(annotation, id.vars = 'Sample')

    if(input$selectCondition1 != 'CUSTOM'){
      sc1 <- annot_long[value == input$selectCondition1, Sample]
    } else {
      sc1
    }

    if(input$selectCondition2 != 'CUSTOM'){
      sc2 <- annot_long[value == input$selectCondition2, Sample]
    } else {
      sc2
    }

    req.cols <- c(sc1, sc2)
    subset_abun <- as.matrix(abundance[, req.cols, with = F])
    row.names(subset_abun) <- abundance$UniProtID
    pheno_data_df <- rbind(data.frame(sample = sc1, condition = 'condition1'),
                           data.frame(sample = sc2, condition = 'condition2'))

    design = model.matrix(~condition, data=pheno_data_df)
    colnames(design) = c("condition1", "condition2")
    fit1  = limma::lmFit(subset_abun, design)
    efit1 = limma::eBayes(fit1)
    rv$toptable <- limma::topTable(efit1, coef="condition2",
                                genelist=row.names(subset_abun),
                                n=Inf, sort.by="p")

    rv$plot <- EnhancedVolcano::EnhancedVolcano(toptable = rv$toptable,
                                                lab = rownames(rv$toptable),
                                                x= 'logFC', y = 'adj.P.Val',
                                                pCutoff = input$pCutoff,
                                                FCcutoff = input$FCcutoff,
                                                drawConnectors = TRUE,
                                                subtitle = '',
                                                title = '',
                                                max.overlaps = 25,
                                                ylab = bquote(~-Log[10] ~ italic(adj.P.Val)))


    output$volcanoPlot <- renderPlot({
      rv$plot
    })
  })

  output$downloadVolcanoPlot <- downloadHandler(
    filename = function() {
      paste("volcano_plot", input$plot_type, sep=".")
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

  output$diffExprResults <- DT::renderDataTable({
    req(rv$toptable)
    DT::datatable(rv$toptable,  filter = "top")
  }, server = FALSE)

  output$downloadDiffExprReport <- downloadHandler(
    filename = function() {
      "Differential_Expression.csv"
    },
    content = function(file) {
      diffexpr_resutls <- as.data.frame(rv$toptable)
      write.csv(diffexpr_resutls, file, row.names=TRUE, quote=FALSE)
    }
  )
}
