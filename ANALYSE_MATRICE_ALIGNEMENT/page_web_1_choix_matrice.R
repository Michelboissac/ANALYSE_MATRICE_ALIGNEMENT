library(shiny)

ui <- fluidPage(
  
  fluidRow(
    column(6,
           div(
             div("Répertoire des fichiers TXT :", style = "font-weight: bold; margin-bottom: 5px;"),
             div(class = "directory-input-group",
                 div(class = "directory-input",
                     textInput("repertoire_fichiers_txt", 
                               label = NULL,
                               value = "", 
                               width = "100%")),
                 div(class = "directory-button",
                     actionButton("bouton_repertoire", 
                                  "📂 Parcourir", 
                                  class = "btn-secondary",
                                  style = "height: 40px;"))
             ),
             div(class = "help-text", "Chemin vers le dossier contenant vos matrices (un / sera ajouté automatiquement à la fin)")
           )
    ),
    column(6,
           textInput("tableau_counts", 
                     label = "Nom de la matrice d'alignements :",
                     value = "", 
                     width = "100%"),
           div(class = "help-text", "Nom de la matrice qui sera analysée (sera rempli automatiquement sans extension .txt)")
    )
  ),
  
  # Bouton de validation
  fluidRow(
    column(12,
           div(style = "text-align: center; margin: 30px 0;",
               actionButton("valider", "valider", 
                            class = "btn-primary btn-lg", 
                            style = "padding: 15px 30px; font-size: 18px;")
           )
    )
  )
)

server <- function(input, output, session) {
  
  # Gestionnaire du bouton de sélection de répertoire/fichier
  observeEvent(input$bouton_repertoire, {
    tryCatch({
      # Utiliser choose.files() sur Windows ou tk_choose.files() comme alternative
      if (.Platform$OS.type == "windows") {
        fichier_selectionne <- choose.files(
          caption = "Sélectionnez un fichier TXT",
          filters = matrix(c("Fichiers TXT", "*.txt", "Tous les fichiers", "*.*"), 
                           nrow = 2, byrow = TRUE)
        )
      } else {
        # Pour Linux/Mac, utiliser tcltk
        if (!requireNamespace("tcltk", quietly = TRUE)) {
          showModal(modalDialog(
            title = "Erreur",
            "Le package 'tcltk' n'est pas disponible. Veuillez saisir le chemin manuellement.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
        fichier_selectionne <- tcltk::tk_choose.files(
          caption = "Sélectionnez un fichier TXT",
          filters = "{{TXT Files} {.txt}} {{All Files} {*}}"
        )
      }
      
      # Si l'utilisateur a sélectionné un fichier
      if (length(fichier_selectionne) > 0 && !is.na(fichier_selectionne) && fichier_selectionne != "") {
        # Extraire le répertoire et le nom du fichier
        repertoire_selectionne <- dirname(fichier_selectionne)
        nom_fichier <- basename(fichier_selectionne)
        nom_sans_extension <- tools::file_path_sans_ext(nom_fichier)
        
        # Normaliser le chemin du répertoire
        chemin_normalise <- normaliser_chemin(repertoire_selectionne)
        
        # Mettre à jour les deux champs
        updateTextInput(session, "repertoire_fichiers_txt", value = chemin_normalise)
        updateTextInput(session, "tableau_counts", value = nom_sans_extension)
        
        # Message de confirmation
        showNotification(
          paste("Fichier sélectionné :", nom_fichier, "dans", chemin_normalise),
          type = "message",
          duration = 4
        )
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Erreur de sélection",
        paste("Impossible d'ouvrir la boîte de dialogue de sélection de fichier.",
              "Erreur :", e$message,
              "Veuillez saisir le chemin et le nom de fichier manuellement."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
  
  
  observeEvent(input$valider, {
    # Récupérer les paramètres
    repertoire_fichiers_txt <<- normaliser_chemin(trimws(input$repertoire_fichiers_txt))
    tableau_counts <<- trimws(input$tableau_counts)
    counts <<- function_recupere_tab_counts_txt(tableau_counts) 
    
    # Attendre 1 seconde puis fermer et sauvegarder
    Sys.sleep(1)
    
    stopApp(returnValue = "menu")  # Retour au menu
  })
}