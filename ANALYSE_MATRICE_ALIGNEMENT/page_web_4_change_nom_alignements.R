library(shiny)

liste <- colnames(counts)

ui <- fluidPage(
  titlePanel("Compléter une liste"),
  
  # Ajout de CSS pour améliorer l'alignement
  tags$head(
    tags$style(HTML("
      .liste-item {
        font-size: 14px;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        margin-bottom: 5px;
        padding: 6px 12px;
        line-height: 1.42857143;
        height: 34px;
        display: flex;
        align-items: center;
        border: 1px solid transparent;
        background-color: #f9f9f9;
      }
      .shiny-input-container {
        margin-bottom: 5px;
      }
      .form-control {
        height: 34px;
        font-size: 14px;
      }
      .chemin-affichage {
        padding: 10px;
        background-color: #e8f4f8;
        border: 1px solid #b8dce8;
        border-radius: 4px;
        margin-top: 10px;
        margin-bottom: 10px;
        font-family: monospace;
        word-break: break-all;
      }
    "))
  ),
  
  # Section de sélection du fichier
  fluidRow(
    column(12,
           h4("Sélection du fichier de correspondance"),
           actionButton("bouton_selection_fichier", "Sélectionner un fichier TXT", 
                        class = "btn-info"),
           uiOutput("affichage_chemin")
    )
  ),
  
  hr(),
  
  fluidRow(
    column(6, 
           h4("Liste"),
           lapply(liste, function(x) 
             tags$div(class = "liste-item", x)
           )
    ),
    column(6, 
           h4("Entrées"),
           lapply(seq_along(liste), function(i) 
             textInput(paste0("entry", i), label = NULL, width = "100%")
           )
    )
  ),
  
  br(),
  actionButton("valider", "Valider", class = "btn-primary"),
  br(), br(),
  tableOutput("resultat")
)

server <- function(input, output, session) {
  
  # Variable réactive pour stocker le chemin
  chemin_fichier <- reactiveVal(NULL)
  
  # Gestionnaire du bouton de sélection de fichier
  observeEvent(input$bouton_selection_fichier, {
    tryCatch({
      # Utiliser choose.files() sur Windows ou tk_choose.files() comme alternative
      if (.Platform$OS.type == "windows") {
        fichier_selectionne <- choose.files(
          caption = "Sélectionnez un fichier TXT de correspondance",
          filters = matrix(c("Fichiers TXT", "*.txt", "Tous les fichiers", "*.*"), 
                           nrow = 2, byrow = TRUE)
        )
      } else {
        # Pour Linux/Mac, utiliser tcltk
        if (!requireNamespace("tcltk", quietly = TRUE)) {
          showModal(modalDialog(
            title = "Erreur",
            "Le package 'tcltk' n'est pas disponible. Impossible d'ouvrir le sélecteur de fichier.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }
        fichier_selectionne <- tcltk::tk_choose.files(
          caption = "Sélectionnez un fichier TXT de correspondance",
          filters = "{{TXT Files} {.txt}} {{All Files} {*}}"
        )
      }
      
      # Si l'utilisateur a sélectionné un fichier
      if (length(fichier_selectionne) > 0 && !is.na(fichier_selectionne) && fichier_selectionne != "") {
        # Normaliser le chemin (utiliser normaliser_chemin si disponible)
        if (exists("normaliser_chemin", mode = "function")) {
          chemin_normalise <- normaliser_chemin(fichier_selectionne)
        } else {
          chemin_normalise <- normalizePath(fichier_selectionne, winslash = "/")
        }
        # Supprimer le slash final s'il existe
        chemin_normalise <- sub("/$", "", chemin_normalise)
        # Stocker le chemin dans la variable réactive
        chemin_fichier(chemin_normalise)
        
        # Message de confirmation
        showNotification(
          paste("Fichier sélectionné :", basename(chemin_normalise)),
          type = "message",
          duration = 4
        )
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Erreur de sélection",
        paste("Impossible d'ouvrir la boîte de dialogue de sélection de fichier.",
              "Erreur :", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
  
  # Affichage dynamique du chemin sélectionné
  output$affichage_chemin <- renderUI({
    if (!is.null(chemin_fichier())) {
      tags$div(
        class = "chemin-affichage",
        tags$strong("Chemin sélectionné : "),
        tags$br(),
        chemin_fichier()
      )
    } else {
      tags$div(
        style = "padding: 10px; color: #888; font-style: italic;",
        "Aucun fichier sélectionné"
      )
    }
  })
  
  observeEvent(input$valider, {
    # Récupération des saisies
    saisies <- sapply(seq_along(liste), function(i) {
      val <- input[[paste0("entry", i)]]
      if(is.null(val) || val == "") "" else val
    })
    
    # Affichage du résultat
    output$resultat <- renderTable({
      data.frame(Liste = liste, Saisi = saisies, stringsAsFactors = FALSE)
    })
    
    # Sauvegarder les résultats dans l'environnement global
    resultats <<- data.frame(Liste = liste, Saisi = saisies, stringsAsFactors = FALSE)
    
    # Sauvegarder le chemin du fichier dans l'environnement global
    if (!is.null(chemin_fichier())) {
      chemin_vers_les_nouveaux_noms_txt <<- chemin_fichier()
    } else {
      chemin_vers_les_nouveaux_noms_txt <<- NULL
    }
    
    # Fermeture automatique de la fenêtre après un court délai
    Sys.sleep(1)
    
    # Fermer la fenêtre
    stopApp(returnValue = "menu")
  })
}

# Lancement de l'application
# shinyApp(ui = ui, server = server)