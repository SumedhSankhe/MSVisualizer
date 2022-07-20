heatmapPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("HeatMap"),
    fluidRow(
      box( title = "Heatmap & Visualization", width = 4, status = "warning",
           tabBox(id = "heatmap", height = "100%", width = "100%",
                  tabPanel("Heatmap",
                           selectInput(inputId = ns('column_col'),
                                       label = 'Color Samples By: ',
                                       choices = c('Disease', 'Condition')
                           ),
                           numericInput(inputId = ns('row_clusters'), label = '# Gene Clusters: ', value='2'),
                           actionButton(ns("heatmapButton"), 
                                        label = "Submit", 
                                        icon("paper-plane"), 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           )
                  ),
                  tabPanel(tagList(shiny::icon("gear"), "Settings"),
                           numericInput(inputId = ns('most_abundant_genes'), label = '# Top Genes (Selected By Abundance): ', value='0'),
                           selectInput(inputId = ns('transform_type'),
                                       label = 'Transform Method: ',
                                       choices = list("None", "log2", "asinh"), # "logicle")
                                       selected = 'None'
                           ),
                           conditionalPanel("input.transform_type == 'asinh'", ns=ns,
                                            numericInput(inputId = ns('cofactor'), label = 'Co-factor: ', value='5')
                           ),
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
        tabBox(id = "displayTab", height = "100%", width = "100%",
          tabPanel("heatmapTab",
            (div(style='width:1000px;overflow-x: scroll;height:1000px;overflow-y: scroll;',
               shinycssloaders::withSpinner(
                 plotOutput(ns("heatmapPlot"), width = "900px", height = "900px"))
            )),
            HTML("<br>"),
            downloadButton(ns('downloadHeatmap'), "Download Plot")
          ),
          tabPanel("Genes-Dendogram",
            (div(style='width:900px;overflow-x: scroll;height:900px;overflow-y: scroll;',
                shinycssloaders::withSpinner(
                  plotOutput(ns("dendogramPlot"), width = "800px", height = "800px"))
            )) #,
            # HTML("<br>"),
            # downloadButton(ns('downloadDendogramPlot'), "Download Plot")
          ),
          tabPanel("Gene-Clusters",
                   dataTableOutput(ns("geneClustersOutput")),
                   HTML("<br>"),
                   downloadButton(ns("downloadClusters"), "Download csv")
          )
        )
      )
    )
  )
}

heatmapPanel <- function(input, output, session, getData) {
  ns <- session$ns
 
  getDataMatrix <- eventReactive( input$heatmapButton, {
    df <- data.frame(getData()[[1]])
    row.names(df) <- df[,1]
    if (input$most_abundant_genes > 0) {
      df <- df %>% arrange(desc(rowMeans(df[,2:ncol(df)]))) %>% top_n(input$most_abundant_genes)
    }
    
    switch (input$transform_type,
      asinh = {
        df[,2:ncol(df)] = asinh(df[2:ncol(df)]/input$cofactor)
      },
      log2 = {
        df[,2:ncol(df)] <- log(df[2:ncol(df)]+1, 2)
      }
    )
    
    df
  })
  
  getGeneClusters <- reactive({
    matrix_df <- getDataMatrix()
    hclust_gene <- hclust(dist(matrix_df), method = "complete")
    gene_clusters <- cutree(tree = as.dendrogram(hclust_gene), k = isolate(input$row_clusters))
    gene_clusters <- data.frame(Gene.Cluster=paste0("cluster-", gene_clusters))
    row.names(gene_clusters) <- row.names(matrix_df)
    gene_clusters
  })
  
  output$geneClustersOutput <- DT::renderDataTable({
    getGeneClusters()
  })

  gen_heatmap <- reactive({
    req(getDataMatrix())
    matrix_df <- getDataMatrix()
    sample_category_df <- data.frame(getData()[[2]])
    sample_category_df$Sample <- gsub("-", ".", sample_category_df$Sample)
    sample_col <- data.frame(SampleType=sample_category_df[isolate(input$column_col)])
    row.names(sample_col) <- sample_category_df$Sample
    hclust_gene <- hclust(dist(matrix_df), method = "complete")
    gene_col <- cutree(tree = as.dendrogram(hclust_gene), k = isolate(input$row_clusters))
    gene_col <- data.frame(Gene.Cluster=paste0("cluster-", gene_col))
    row.names(gene_col) <- row.names(matrix_df)
    p <- pheatmap(matrix_df[,2:ncol(matrix_df)], cluster_rows = T, 
                  show_rownames = F, treeheight_row = 0,
                  annotation_row = gene_col, annotation_col = sample_col)
    return(p)    
  })
  
  output$heatmapPlot <- renderPlot({
    req(getDataMatrix())
    p <- gen_heatmap()
    p
  })
  
  # output$downloadHeatmap <- downloadHandler(
  #   filename = "heatmap_plot.svg",
  #   content = function(file) {
  #     ggsave(file, plot = gen_heatmap(), device = "svg", width = input$plot_width, height = input$plot_height, units="in")
  #   }
  # )

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
      plot = gen_heatmap()
      print(plot)
      dev.off()
    }
  )
  
  gen_dendogramplot <- reactive({
    req(getDataMatrix())
    df <- getDataMatrix()
    hclust_gene <- hclust(dist(df), method = "complete")
    p <- as.dendrogram(hclust_gene) %>% plot(horiz = TRUE)
    return(p)
  })
  
  output$dendogramPlot <- renderPlot({
    req(getDataMatrix())
    p <- gen_dendogramplot()
    return(p)
  })

  # output$downloadDendogramPlot <- downloadHandler(
  #   filename = "dendogram_plot.svg",
  #   content = function(file) {
  #     ggsave(file, plot = gen_dendogramplot(), device = "svg")
  #   }
  # )
    
  output$downloadClusters <- downloadHandler(
    filename = function() {
      "Gene_clusters.csv"
    },
    content = function(file) {
      gene_clusters <- getGeneClusters()
      write.csv(gene_clusters, file, row.names=TRUE, quote=FALSE)
    }
  ) 
}
