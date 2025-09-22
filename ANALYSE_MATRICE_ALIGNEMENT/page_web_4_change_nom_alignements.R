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
    "))
  ),
  
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
    
    # Fermeture automatique de la fenêtre après un court délai
    Sys.sleep(1)  # Pause d'1 seconde pour voir le résultat
    
    # Sauvegarder les résultats dans l'environnement global
    resultats <<- data.frame(Liste = liste, Saisi = saisies, stringsAsFactors = FALSE)
    
    # Fermer la fenêtre
    stopApp(returnValue = "menu")
  })
}

# Lancement de l'application
