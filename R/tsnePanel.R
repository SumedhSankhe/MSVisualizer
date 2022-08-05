tsnePanelUI <- function(id) {
  ns <- NS(id)
  tagList(
           numericInput(inputId = ns('perplexity'), label = 'Perplexity: ', value='15'),
           sliderInput(inputId = ns('theta'), label = 'Theta: ', value='0.5', min = 0.0, max = 1.0),
           numericInput(inputId = ns('max_iter'), label = 'Maximum Iter: ', value='1000'),
           numericInput(inputId = ns('eta'), label = 'Learning Rate: ', value='200'),
           #numericInput(inputId = ns('num_threads'), label = 'Num Threads: ', value='0'),
           numericInput(inputId = ns('dims'), label = 'Output Dimensionality: ', value='2')
  )
}

tsnePanel <- function(input, output, session, abundance, smarkers, annotations, proj_dims) {
  ns <- session$ns

  compute_tsne_projs <- reactive({
    if(length(smarkers)!=0 | smarkers != '' | !is.null(smarkers)){
      abundance <- abundance[, smarkers, with = F]
    }
    abundance <- abundance[complete.cases(abundance),]
    dups <- duplicated(abundance)
    abundance <- abundance[!dups,]
    ts_out <- Rtsne::Rtsne(X = as.matrix(abundance), dims = input$dims,
                           perplexity = input$perplexity, theta = input$theta,
                           eta = input$eta, max_iter = input$max_iter)


    data_plot <- ts_out$Y[,c(proj_dims()[1], proj_dims()[2])]
    row.names(data_plot) <- rownames(abundance)
    data_plot <- as.data.table(data_plot, keep.rownames = TRUE)
    setnames(data_plot, names(data_plot), c('rn',paste0('tSNE_',proj_dims())))


    anno_sub <- annotations[Sample %in% data_plot$rn]
    abundance <- as.data.table(abundance, keep.rownames = T)
    setnames(abundance,'rn', 'Sample')

    joined_df <- abundance[anno_sub, on = 'Sample']
    list(projInput=joined_df, projOutput=data_plot)
  })

  return(compute_tsne_projs)
}
