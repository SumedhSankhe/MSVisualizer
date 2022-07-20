#' @export

runApp <- function(){
  appDir <- system.file('shiny', package = 'MSdataVisualizer')
  if(appDir == ''){
    stop('Doink')
  }

  shiny::runApp(appDir = appDir, display.mode = 'normal', launch.browser = TRUE)
}
