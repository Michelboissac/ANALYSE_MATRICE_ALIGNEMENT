rm(list=ls())
library(shiny)
source("librairies.R")
source("fonctions.R")
counts = data.frame()

# Fonction pour lancer une page
lancer_page <- function(page) {
  if (page == "menu" || page == "accueil") {
    source("page_web_0_principale.R", local = FALSE)
    return(runApp(shinyApp(ui = ui, server = server)))
    
  } else if (page == "matrice") {
    source("page_web_1_choix_matrice.R", local = FALSE)
    return(runApp(shinyApp(ui = ui, server = server)))

  } else if (page == "genes") {
    
    
    source("page_web_2_choix_genes.R", local = FALSE) 
    return(runApp(shinyApp(ui = ui, server = server)))
    
  } else if (page == "normalisation") {
    
    source("page_web_3_choix_normalisation.R", local = FALSE)
    return(runApp(shinyApp(ui = ui, server = server)))
    
  }else if (page == "noms_alignements") {
    source("creation_repertoire.R")
    source("page_web_4_change_nom_alignements.R")
    return(runApp(shinyApp(ui = ui, server = server)))
  }
  
  else if (page == "analyses") {
    source("change_noms_alignements.R")
    source("normalisation.R")
    source("page_web_5_analyse.R", local = FALSE)
    return(runApp(shinyApp(ui = ui, server = server)))
  }
  else if (page == "expr_diff") {
    source("page_web_5_analyse_express_diff.R", local = FALSE)
    return(runApp(shinyApp(ui = ui, server = server)))
  }
  return(NULL)
}

# Boucle principale de navigation
page_courante <- "menu"  # Commencer par le menu principal

while (!is.null(page_courante) && page_courante != "quit") {
  cat("-> Lancement de:", page_courante, "\n")
  tryCatch({
    resultat <- lancer_page(page_courante)
    page_courante <- resultat
    cat("-> Page fermée, résultat:", resultat, "\n")
  }, error = function(e) {
    cat("ERREUR lors du lancement de", page_courante, ":", e$message, "\n")
    page_courante <- "menu"  # Retour au menu en cas d'erreur
  })
}