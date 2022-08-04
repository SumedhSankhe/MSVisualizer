heatmapPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("HeatMap"),
    fluidRow(
      box(title = "Control Box", status = "warning", height = '100%',
          width = 5,
          selectInput(inputId = ns('column_col'),
                      label = 'Color Samples By: ',
                      choices = c('Disease', 'Condition')
          ),
          numericInput(inputId = ns('row_clusters'),
                       label = '# Gene Clusters: ', value='2'),
          numericInput(inputId = ns('most_abundant_genes'),
                       label = '# Top Genes (Selected By Abundance): ',
                       value='0'),
          selectInput(inputId = ns('transform_type'),
                      label = 'Transform Method: ',
                      choices = list("None", "log2", "asinh"), # "logicle")
                      selected = 'None'
          ),
          conditionalPanel("input.transform_type == 'asinh'",
                           ns=ns,
                           numericInput(inputId = ns('cofactor'),
                                        label = 'Co-factor: ',
                                        value='5')
          ),
          actionButton(ns("heatmapButton"),
                       label = "Submit",
                       icon("paper-plane"),
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )),
      box(title = "Download Settings", status = "warning", height = '100%',
          width = 5,
          numericInput(inputId = ns('plot_width'),
                       label = 'Plot Width: ',
                       value='10'),
          numericInput(inputId = ns('plot_height'),
                       label = 'Plot Height: ',
                       value='10'),
          selectInput(inputId = ns('plot_type'),
                      label = 'Plot Type: ',
                      choices = c('svg', 'png', 'pdf'))
      )
    ),
    fluidRow(
      box(title = "Visualization", status = 'warning', height = '100%',
          width = 12,
          tabBox(width = 12,
            tabPanel(title = "Heatmap",
                     (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                          shinycssloaders::withSpinner(
                            plotOutput(ns("heatmapPlot"))),
                          downloadButton(ns('downloadHeatmap'), "Download Plot")
                     ))),
            tabPanel("Genes-Dendogram",
                     (div(style='width:auto;overflow-x: scroll;height:auto;overflow-y: scroll;',
                          shinycssloaders::withSpinner(
                            plotOutput(ns("dendogramPlot")))
                     ))
            ),
            tabPanel("Gene-Clusters", width = '100%',
                     dataTableOutput(ns("geneClustersOutput")),
                     downloadButton(ns("downloadClusters"), "Download csv")
            )
          )
      )
    )
  )
}

heatmapPanel <- function(input, output, session, getData) {
  ns <- session$ns
  rv <- reactiveValues()

  observeEvent(input$heatmapButton, {
    req(!is.null(getData()))

    abundance <- getData()[[1]]
    #get most abundant gene, if provided
    if (input$most_abundant_genes > 0) {
      most_abun <- abundance[, rowMeans(.SD), by=UniProtID][order(-V1), head(.SD, input$most_abundant_genes)][, UniProtID]
      abundance <- abundance[UniProtID %in% most_abun]
    }
    #tranform data if not none
    if(input$transform_type != 'None'){
      abundance <- transform_data(abundance, arg = input$transform_type)
    }

    rv$gene_clusters <- hierarchial_clustering(dt = abundance,
                                               clusters = isolate(input$row_clusters))

    rv$abun_matrix <-as.matrix(abundance[,-'UniProtID'])
    row.names(rv$abun_matrix) <- abundance$UniProtID
    annotations <- getData()[[2]]

    rv$sample_col <- create_annotation_col(ann = annotations,
                                           col = isolate(input$column_col))

    output$heatmapPlot <-  renderPlot({
      pheatmap::pheatmap(rv$abun_matrix, cluster_rows = TRUE,
               show_rownames = F, treeheight_row = 0,
               annotation_col = rv$sample_col,
               annotation_row = rv$gene_clusters$clusters,
               annotation_names_col = F)
    })

    output$dendogramPlot <- renderPlot({
      # nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),
      #                 cex = 0.7, col = "blue")
      plot(rv$gene_clusters$dendogram,  #nodePar = nodePar,
           horiz = T)
    })

    output$geneClustersOutput <- DT::renderDataTable({
      rv$gene_clusters$clusters
    })

  })

  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste("gen_heatmap", input$plot_type, sep=".")
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
      pheatmap::pheatmap(rv$abun_matrix, cluster_rows = TRUE,
                         show_rownames = F, treeheight_row = 0,
                         annotation_col = rv$sample_col,
                         annotation_row = rv$gene_clusters$clusters,
                         annotation_names_col = F)
      dev.off()
    }
  )

  output$downloadClusters <- downloadHandler(
    filename = function() {
      "Gene_clusters.csv"
    },
    content = function(file) {
      gene_clusters <- getGeneClusters()
      write.csv(rv$gene_clusters$clusters, file, row.names=TRUE, quote=FALSE)
    }
  )
}


transform_data <- function(dt, arg){
  #df[,2:ncol(df)] = asinh(df[2:ncol(df)]/input$cofactor)
  # TODO: what is cofactor? why is used? why is it important for this
  rapply(dt, get(arg), classes = 'numeric', how = 'replace')
}

hierarchial_clustering <- function(dt, clusters){
  clust <- hclust(dist(dt[,-'UniProtID']), method = "complete")
  gene_clusters <- dendextend::cutree(tree = as.dendrogram(clust), k =clusters)
  gene_clusters <- data.frame(Gene.Cluster=paste0("cluster-", gene_clusters))
  row.names(gene_clusters) <- dt[,UniProtID]
  list('clusters'=gene_clusters, 'dendogram' = as.dendrogram(clust))
}

