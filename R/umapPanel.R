umapPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(inputId = ns('min_dist'), label = 'Minimum Distance (min_dist): ', value='0.1'),
    numericInput(inputId = ns('n_components'), label = 'Output Dimensionality (n_components): ', value='2'),
    numericInput(inputId = ns('n_neighbors'), label = 'Num Neighbors (n_neighbors): ', value='15'),
    textInput(inputId = ns('metric'), label = 'Metric (metric): ', value='euclidean')

  )
}

umapPanel <- function(input, output, session, abundance, smarkers, annotations, proj_dims) {
  ns <- session$ns

  compute_umap_projs <- reactive({

    if( !is.null(smarkers)){
      abundance <- abundance[, smarkers, with = F]
    }
    abundance <- abundance[complete.cases(abundance),]
    dups <- duplicated(abundance)
    abundance <- abundance[!dups,]

    umap_out <- umap::umap(
      d = abundance, n_components=input$n_components,
      n_neighbors=input$n_neighbors, min_dist=input$min_dist,
      metric=input$metric
    )

    umap_proj <- as.data.table(umap_out$layout, keep.rownames = T)
    col_names <- c('Sample', paste0('PROJ_', proj_dims()))
    setnames(umap_proj, names(umap_proj), col_names)

    anno_sub <- annotations[Sample %in% umap_proj$Sample]
    abundance <- as.data.table(abundance, keep.rownames = T)
    setnames(abundance, 'rn', 'Sample')


    list(projOutput=umap_proj[anno_sub, on = 'Sample'],
         projInput= abundance[anno_sub, on = 'Sample'])
  })

  return(compute_umap_projs)
}
