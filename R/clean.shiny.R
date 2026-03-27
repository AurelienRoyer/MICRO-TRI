clean_shiny <- function(x) {
  if (inherits(x, "reactivevalues")) {
    x <- shiny::reactiveValuesToList(x)
  }
  
  if (is.list(x)) {
    x <- lapply(x, clean_shiny)
  }
  
  return(x)
}