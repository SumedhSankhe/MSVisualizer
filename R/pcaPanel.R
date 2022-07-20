#' @export
pcaPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(inputId = ns('num_threads'), label = 'Num Threads: ', value='1')
  )
}

#' @export
pcaPanel <- function(input, output, session, getData, smarkers, getPhenoData) {
  ns <- session$ns

  compute_pca_projs <- reactive({
    if ( length(smarkers()) == 0) {
      select_df <- getData()
    } else {
      select_df <- getData() %>% dplyr::select(smarkers())
    }
    #select_df <- cbind(select_df, getPhenoData()[-1])
    
    select_df <- select_df[complete.cases(select_df),]
    #select_df <- select_df[, !( names(joined_df) %in% names(getPhenoData()[-1]))]

    select_df <- select_df[rowSums(select_df)>0, ]
    select_df <- select_df %>% dplyr::select( -nearZeroVar(select_df) )
    pca <- prcomp(select_df, scale. = TRUE)
    scores <- as.data.frame(pca$x)
    #input_df <- getData()
    #filtered_input_df <- subset(input_df, as.numeric(row.names(input_df)) %in% as.numeric(row.names(select_df)))
    phenoTable <- getPhenoData()
    phenoTable <- subset(phenoTable, Sample %in% row.names(scores))
    joined_df <- cbind(select_df, phenoTable[-1])
    #return( list(projOutput=scores[, c("PC1","PC2")], projInput=joined_df) ) #filtered_input_df) )
    return(list(projOutput=pca, projInput=joined_df))
  })
  return(compute_pca_projs)
}
