microTRI <- function(){
  shiny::addResourcePath('www', system.file('www', package = 'microTRI'))
  
  shinyApp(ui = app_ui, server = app_server)
}
