umapPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(inputId = ns('min_dist'), label = 'Minimum Distance (min_dist): ', value='0.1'),
    numericInput(inputId = ns('n_components'), label = 'Output Dimensionality (n_components): ', value='2'),
    numericInput(inputId = ns('n_neighbors'), label = 'Num Neighbors (n_neighbors): ', value='15'),
    textInput(inputId = ns('metric'), label = 'Metric (metric): ', value='euclidean')
    
  )
}

umapPanel <- function(input, output, session, getData, smarkers, getPhenoData, proj_dims) {
  ns <- session$ns
  
  compute_umap_projs <- reactive({
    if ( length(smarkers()) == 0) {
      select_df <- getData()
    } else {
      select_df <- getData() %>% dplyr::select(smarkers())
    }
    select_df <- select_df[complete.cases(select_df),]  
    df.umap <- umap(select_df, n_components=input$n_components, n_neighbors=input$n_neighbors, min_dist=input$min_dist, metric=input$metric)
    umap_proj_df <- as.data.frame( df.umap$layout[,c(proj_dims()[1],proj_dims()[2])] )
    colnames(umap_proj_df) <- c(paste0("PROJ", proj_dims()[1]), paste0("PROJ", proj_dims()[2]))
    #input_df <- getData()
    #filtered_input_df <- subset(input_df, as.numeric(row.names(input_df)) %in% as.numeric(row.names(select_df)))
    phenoTable <- getPhenoData()
    phenoTable <- subset(phenoTable, Sample %in% row.names(umap_proj_df))
    joined_df <- cbind(select_df, phenoTable[-1])
    return( list(projOutput=umap_proj_df, projInput=joined_df) )
  })
  
  return(compute_umap_projs)
}
