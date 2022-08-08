#' @export
pcaPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(inputId = ns('num_threads'), label = 'Num Threads: ', value='1')
  )
}

#' @export
pcaPanel <- function(input, output, session, abundance, smarkers, annotations) {
  ns <- session$ns

  compute_pca_projs <- reactive({

    if(!is.null(smarkers)){
      abundance <- abundance[, smarkers, with = F]
    }
    abundance <- abundance[complete.cases(abundance),]
    dups <- duplicated(abundance)
    abundance <- abundance[!dups,]
    abundance <- abundance[rowSums(abundance)>0,]
    abundance <- abundance[-caret::nearZeroVar(abundance)]

    pca_out <- prcomp(abundance, scale. = TRUE)
    scores <- as.data.table(pca_out$x,keep.rownames = T)
    setnames(scores, 'rn','Sample')

    anno_sub <- annotations[Sample %in% scores$Sample]
    abundance <- as.data.table(abundance, keep.rownames = T)
    setnames(abundance, 'rn', 'Sample')
    joined_df <- abundance[anno_sub, on = 'Sample']
    scores <- scores[anno_sub, on = 'Sample']

    list(projOutput=scores, pca = pca_out, projInput=joined_df)
  })
  return(compute_pca_projs)
}
