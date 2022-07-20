tsnePanelUI <- function(id) {
  ns <- NS(id)
  tagList(
           numericInput(inputId = ns('perplexity'), label = 'Perplexity: ', value='15'),
           sliderInput(inputId = ns('theta'), label = 'Theta: ', value='0.5', min = 0.0, max = 1.0),
           numericInput(inputId = ns('max_iter'), label = 'Maximum Iter: ', value='1000'),
           numericInput(inputId = ns('eta'), label = 'Learning Rate: ', value='200'),
           numericInput(inputId = ns('num_threads'), label = 'Num Threads: ', value='0'),
           numericInput(inputId = ns('dims'), label = 'Output Dimensionality: ', value='2')
  )
}

tsnePanel <- function(input, output, session, getData, smarkers, getPhenoData, proj_dims) {
  ns <- session$ns
  
  compute_tsne_projs <- reactive({
    if ( length(smarkers()) == 0) {
      select_df <- getData()
    } else {
      select_df <- getData() %>% dplyr::select(smarkers())
    }
    select_df <- select_df[complete.cases(select_df),]
    dups <- duplicated(select_df)
    data_Rtsne <- select_df[!dups, ]

    tsne_out <- Rtsne( as.matrix(data_Rtsne), dims = input$dims, perplexity = input$perplexity, theta = input$theta, eta = input$eta, max_iter = input$max_iter, num_threads = input$num_threads )
    data_plot <- as.data.frame(tsne_out$Y[,c(proj_dims()[1],proj_dims()[2])])
    colnames(data_plot) <- c(paste0("tSNE_", proj_dims()[1]), paste0("tSNE_", proj_dims()[2]))
    row.names(data_plot) <- row.names(data_Rtsne)
    #input_df <- getData()
    #filtered_input_df <- subset(input_df, as.numeric(row.names(input_df)) %in% as.numeric(row.names(data_Rtsne)))
    phenoTable <- getPhenoData()
    phenoTable <- subset(phenoTable, Sample %in% row.names(data_plot))
    joined_df <- cbind(data_Rtsne, phenoTable[-1])
    return( list(projInput=joined_df, projOutput=data_plot) )
  })
  
  return(compute_tsne_projs)
}
