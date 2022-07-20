loadPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Load Data"),
    fluidRow(
      box( title = "Data upload & settings", width = 4, status = "warning",
           tabBox(id = "loadtab", height = "100%", width = "100%",
                  tabPanel("Upload",
                           fileInput( ns('localfile'), label = "Choose file to upload", accept = c('.xls', '.xlsx') ),
                           div(style = "margin-top:-25px"),
                           a(href="MSdata.xlsx", "Example: MSdata.xlsx", download=NA, target="_blank")
                  )
           )
      ),
      box(
        status = "warning", width = 8,
        tabBox(id = "displayTab", height = "100%", width = "100%",
               tabPanel("Matrix Stats",
                        dataTableOutput(ns("tableOutput")),
                        br(),
                        br()
               ),
               tabPanel("Upset By Group",
                        dataTableOutput(ns("groupOutput"))
               ),
               tabPanel("Upset By Sample",
                        dataTableOutput(ns("sampleOutput"))
               ),
               tabPanel("Classification",
                        dataTableOutput(ns("classificationOutput"))
               )
        )
      )
    )
  )
}

loadPanel <- function(input, output, session) {
  countsData <- reactive({
    req(input$localfile)
    data_list <- lapply(excel_sheets(input$localfile$datapath), read_excel, path=input$localfile$datapath)
    #df <- read.csv(input$localfile$datapath, sep=",", row.names = NULL, header=TRUE, stringsAsFactors = FALSE)
    colnames(data_list[[1]]) <- make.names(colnames(data_list[[1]]))
    data_list[[2]]$Sample <- make.names(data_list[[2]]$Sample)
    data_list[[2]]$Condition <- make.names(data_list[[2]]$Condition)
    data_list[[2]]$Disease <- make.names(data_list[[2]]$Disease)
    return(data_list)
  })
  
  output$tableOutput <- DT::renderDataTable({
    req(input$localfile)
    DT::datatable(data.frame(countsData()[[1]]), options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$groupOutput <- DT::renderDataTable({
    req(input$localfile)
    upset_by_group <- get_upset_by_group(countsData())
    DT::datatable(upset_by_group, options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$sampleOutput <- DT::renderDataTable({
    req(input$localfile)
    upset_by_sample <- get_upset_by_sample(countsData())
    DT::datatable(upset_by_sample, options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$classificationOutput <- DT::renderDataTable({
    req(input$localfile)
    DT::datatable(data.frame(countsData()[[2]]), options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  return(countsData)
}

get_upset_by_sample <- function(data_list) {
  matrix_stats <- data.frame(data_list[[1]])
  row.names(matrix_stats) <- matrix_stats$UniProtID
  matrix_stats <- matrix_stats[,2:ncol(matrix_stats)]
  upset_by_sample <- floor(matrix_stats)
  upset_by_sample[upset_by_sample > 0] <- 1
  return(upset_by_sample)
}

get_upset_by_group <- function(data_list) {
  sample_category_df <- data.frame(data_list[[2]])
  sample_category_df$Sample <- gsub("-", ".", sample_category_df$Sample)
  diseases <- unique(sample_category_df$Disease)
  upset_by_sample <- get_upset_by_sample(data_list)
  upset_by_group <- data.frame( matrix(ncol=length(diseases), nrow = nrow(upset_by_sample)) )
  colnames(upset_by_group) <- diseases
  rownames(upset_by_group) <- rownames(upset_by_sample)
  for (disease in diseases ) {
    col_names <- sample_category_df[sample_category_df$Disease == disease, "Sample"]
    if( length(col_names) > 1) {
      upset_by_group[disease] <- rowSums(upset_by_sample[, col_names])
    } else {
      upset_by_group[disease] <- upset_by_sample[, col_names]
    }
  }
  upset_by_group[upset_by_group > 0] <- 1
  return(upset_by_group)
}