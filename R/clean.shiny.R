clean_shiny <- function(x) {
  
 
  if (is.data.frame(x)) {
    return(x)
  }
  

  if (is.atomic(x)) {
    return(x)
  }
  
 
  if (inherits(x, "reactivevalues")) {
    x <- shiny::reactiveValuesToList(x)
  }
  
 
  if (is.list(x)) {
    x <- lapply(x, clean_shiny)
    
  
    class(x) <- "list"
  }
  
  return(x)
}


safe_update_rv <- function(rv, lst) {
  
  # sécuriser lst
  if (is.null(lst) || length(lst) == 0) {
    message("lst vide → rien à charger")
    return(rv)
  }
  
  # transformer reactiveValues → liste
  if (inherits(lst, "reactivevalues")) {
    lst <- reactiveValuesToList(lst)
  }
  
  # vérifier que c'est une liste
  if (!is.list(lst)) {
    message("lst n'est pas une liste → ignoré")
    return(rv)
  }
  
  # sécuriser rv
  if (!inherits(rv, "reactivevalues")) {
    new_rv <- reactiveValues()
    
    if (is.list(rv) && length(rv) > 0) {
      for (n in names(rv)) {
        new_rv[[n]] <- rv[[n]]
      }
    }
    
    rv <- new_rv
  }
  
  # sécuriser noms
  nms <- names(lst)
  
  if (is.null(nms) || length(nms) == 0) {
    message("pas de noms dans lst → rien à injecter")
    return(rv)
  }
  
  # injection
  for (n in nms) {
    if (!is.null(n) && nzchar(n)) {
      rv[[n]] <- lst[[n]]
    }
  }
  
  return(rv)
}
