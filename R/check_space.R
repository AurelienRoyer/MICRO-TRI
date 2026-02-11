#' Vérifie si au moins 50 Mo d'espace sont disponibles dans un répertoire.
#'
#' @param path Chemin du répertoire à vérifier (par défaut, le répertoire temporaire).
#' @return TRUE si l'espace est suffisant, FALSE sinon.
check_disk_space <- function(path = tempdir()) {
  file_size <- 50 * 1024^2  # 50 Mo en octets
  temp_file <- file.path(path, "test_50mo.tmp")
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Ouvrir le fichier en écriture binaire
  con <- file(temp_file, "wb")
  on.exit(close(con))  # Fermer le fichier à la sortie
  
  result <- tryCatch(
    {
      # Écrire le fichier par blocs de 1 Mo pour éviter les problèmes de mémoire
      block_size <- 1024^2  # 1 Mo
      for (i in seq(1, file_size, by = block_size)) {
        end <- min(i + block_size - 1, file_size)
        #char_vector <- rep("A", end - i + 1)  # Vecteur de caractères de longueur 1
        char_vector <- strrep("A", end - i + 1)
        writeBin(charToRaw(char_vector), con)
      }
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  
  return(result)

}
