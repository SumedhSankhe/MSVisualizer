loadPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Load Data"),
    fluidRow(
      box( title = "Data upload & settings", width = 4, status = "warning",
           tabBox(id = "loadtab", height = "100%", width = "100%",
                  tabPanel("Upload",
                           fileInput( ns('localfile'),
                                      label = "Choose file to upload",
                                      accept = c('.xls', '.xlsx') ),
                           div(style = "margin-top:-25px"),
                           a(href="MSdata.xlsx", "Example: MSdata.xlsx",
                             download=NA, target="_blank")
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
               )))
    )
  )
}

loadPanel <- function(input, output, session) {

  countsData <- reactive({
    req(input$localfile)
    path <- input$localfile$datapath
    #get sheets information
    sheets <- readxl::excel_sheets(path = path)
    # read each sheet sequentially as a data.table in the data_list object
    data_list <- lapply(sheets, function(x){
      as.data.table(
        readxl::read_excel(path = path, sheet = x)
      )
    })

    #sanitize column names and other name variables
    colnames(data_list[[1]]) <- make.names(colnames(data_list[[1]]))
    data_list[[2]]$Sample <- make.names(data_list[[2]]$Sample)
    data_list[[2]]$Condition <- make.names(data_list[[2]]$Condition)
    data_list[[2]]$Disease <- make.names(data_list[[2]]$Disease)
    return(data_list)
  })

  #render the stats about the read in file
  output$tableOutput <- DT::renderDataTable({
    req(input$localfile)
    DT::datatable(countsData()[[1]], options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })

  #upset by group data output
  output$groupOutput <- DT::renderDataTable({
    req(input$localfile)
    upset_by_group <- get_upset_by_group(countsData()[[2]], countsData()[[1]])
    DT::datatable(upset_by_group, options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })

  #upset by sample data output
  output$sampleOutput <- DT::renderDataTable({
    req(input$localfile)
    DT::datatable(get_upset_by_sample(matrix_stats = countsData()[[1]]),
                  options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })

  #
  output$classificationOutput <- DT::renderDataTable({
    req(input$localfile)
    DT::datatable(countsData()[[2]], options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })

  return(countsData)
}




get_upset_by_sample <- function(matrix_stats) {
  dt <- rapply(matrix_stats, floor, classes = 'numeric', how = 'replace')
  idx <- which(names(dt) == 'UniProtID')
  dt[, names(dt)[-idx] := lapply(.SD, function(x) as.integer(x!=0)), .SDcols = 2:ncol(dt)]
  dt
}

get_upset_by_group <- function(dt, dt2) {
  dt$Sample <- gsub("-", ".", dt$Sample)
  diseases <- unique(dt$Disease)
  upset_by_sample <- get_upset_by_sample(matrix_stats = dt2)

  dt_list <- lapply(diseases, function(x){
    col_names <- dt[Disease ==x, Sample]
    ubg <- upset_by_sample[, rowSums(.SD), .SDcols = col_names, by = UniProtID]
    setkeyv(ubg, 'UniProtID')
    ubg[, V1:= ifelse(V1>0,1,0)]
    names(ubg)[2] <- x
    ubg
  })

  upset_by_group <- Reduce(function(...) merge(..., all = TRUE), dt_list)
  upset_by_group
}
