library(shiny)
library(shinydashboard)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Analyse Expression Différentielle"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sélection des conditions", tabName = "selection", icon = icon("cog"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "selection",
              fluidRow(
                box(
                  title = "Paramètres d'analyse", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  
                  h4("Sélection des conditions à comparer"),
                  
                  # Sélection condition 1
                  div(
                    style = "margin-bottom: 20px;",
                    h5("Condition 1:", style = "font-weight: bold; color: #3c8dbc;"),
                    selectInput(
                      inputId = "condition_1",
                      label = NULL,
                      choices = NULL, # Sera mis à jour dynamiquement
                      selected = NULL,
                      width = "100%"
                    )
                  ),
                  
                  # Sélection condition 2
                  div(
                    style = "margin-bottom: 30px;",
                    h5("Condition 2:", style = "font-weight: bold; color: #3c8dbc;"),
                    selectInput(
                      inputId = "condition_2",
                      label = NULL,
                      choices = NULL, # Sera mis à jour dynamiquement
                      selected = NULL,
                      width = "100%"
                    )
                  ),
                  
                  # Message d'information
                  div(
                    style = "background-color: #f4f4f4; padding: 10px; border-radius: 5px; margin-bottom: 20px;",
                    icon("info-circle"), 
                    span("Sélectionnez deux conditions différentes pour effectuer l'analyse d'expression différentielle.")
                  ),
                  
                  # Bouton de validation
                  div(
                    style = "text-align: center;",
                    actionButton(
                      inputId = "valider",
                      label = "Lancer l'analyse",
                      class = "btn-primary btn-lg",
                      style = "width: 200px; margin-top: 10px;",
                      icon = icon("play")
                    )
                  )
                ),
                
                # Boîte d'information sur le côté
                box(
                  title = "Information", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  
                  h5("Conditions disponibles:"),
                  verbatimTextOutput("conditions_info"),
                  
                  hr(),
                  
                  h5("Sélection actuelle:"),
                  verbatimTextOutput("selection_actuelle")
                )
              ),
              
              # Zone de messages/résultats
              fluidRow(
                box(
                  title = "Messages et résultats", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  verbatimTextOutput("messages")
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Vérifier que 'counts' existe et mettre à jour les choix
  observe({
    if (exists("counts", envir = .GlobalEnv)) {
      counts <- get("counts", envir = .GlobalEnv)
      liste_conditions <- sub("\\..*$", "", colnames(counts))
      liste_conditions <- liste_conditions[!duplicated(liste_conditions)]
      
      updateSelectInput(session, "condition_1",
                        choices = liste_conditions,
                        selected = if(length(liste_conditions) > 0) liste_conditions[1] else NULL)
      
      updateSelectInput(session, "condition_2",
                        choices = liste_conditions,
                        selected = if(length(liste_conditions) > 1) liste_conditions[2] else NULL)
    } else {
      showNotification("Erreur: L'objet 'counts' n'existe pas dans l'environnement global.", 
                       type = "error", duration = 10)
    }
  })
  
  # Afficher les conditions disponibles
  output$conditions_info <- renderText({
    if (exists("counts", envir = .GlobalEnv)) {
      conditions <- colnames(get("counts", envir = .GlobalEnv))
      paste(conditions, collapse = "\n")
    } else {
      "Aucune donnée 'counts' trouvée"
    }
  })
  
  # Afficher la sélection actuelle
  output$selection_actuelle <- renderText({
    if (!is.null(input$condition_1) && !is.null(input$condition_2)) {
      paste("Condition 1:", input$condition_1, "\nCondition 2:", input$condition_2)
    } else {
      "Aucune sélection"
    }
  })
  
  # Messages de sortie
  values <- reactiveValues(messages = "Prêt pour l'analyse...")
  
  output$messages <- renderText({
    values$messages
  })
  
  # Action du bouton valider
  observeEvent(input$valider, {
    
    # Validation des entrées
    if (is.null(input$condition_1) || is.null(input$condition_2)) {
      values$messages <- "Erreur: Veuillez sélectionner les deux conditions."
      showNotification("Veuillez sélectionner les deux conditions.", type = "error")
      return()
    }
    
    if (input$condition_1 == input$condition_2) {
      values$messages <- "Erreur: Les deux conditions doivent être différentes."
      showNotification("Les deux conditions doivent être différentes.", type = "error")
      return()
    }
    
    # Vérifier que les objets nécessaires existent
    objets_requis <- c("counts", "function_analyse_expression_differentielle", 
                       "chemin_repertoire_output", "noms_genes")
    objets_manquants <- objets_requis[!sapply(objets_requis, exists, envir = .GlobalEnv)]
    
    if (length(objets_manquants) > 0) {
      message_erreur <- paste("Erreur: Objets manquants dans l'environnement global:",
                              paste(objets_manquants, collapse = ", "))
      values$messages <- message_erreur
      showNotification(message_erreur, type = "error", duration = 10)
      return()
    }
    
    # Exécution de l'analyse
    values$messages <- paste("Démarrage de l'analyse...\n",
                             "Condition 1:", input$condition_1, "\n",
                             "Condition 2:", input$condition_2, "\n",
                             "Traitement en cours...")
    
    tryCatch({
      # Récupération des variables depuis l'environnement global
      counts <- get("counts", envir = .GlobalEnv)
      chemin_repertoire_output <- get("chemin_repertoire_output", envir = .GlobalEnv)
      noms_genes <- get("noms_genes", envir = .GlobalEnv)
      function_analyse_expression_differentielle <- get("function_analyse_expression_differentielle", envir = .GlobalEnv)
      
      # Affichage des colonnes comme dans votre code original
      print(colnames(counts))
      
      # Assignation des conditions sélectionnées
      condition_1 <- input$condition_1
      condition_2 <- input$condition_2
      
      # Exécution de la fonction d'analyse
      function_analyse_expression_differentielle(        
        counts = counts,
        condition_1 = condition_1,
        condition_2 = condition_2,
        repertoire = paste0(chemin_repertoire_output, "/analyse_expression_differentielle_", 
                            condition_1, "_vs_", condition_2, "_", noms_genes),
        noms_genes = noms_genes
      )
      
      # Message de succès
      values$messages <- paste("Analyse terminée avec succès!\n",
                               "Condition 1:", condition_1, "\n",
                               "Condition 2:", condition_2, "\n",
                               "Répertoire de sortie:", 
                               paste0(chemin_repertoire_output, "/analyse_expression_differentielle_", 
                                      condition_1, "_vs_", condition_2, "_", noms_genes))
      
      showNotification("Analyse d'expression différentielle terminée avec succès!", 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      message_erreur <- paste("Erreur lors de l'exécution:", e$message)
      values$messages <- message_erreur
      showNotification(message_erreur, type = "error", duration = 10)
    })
  })
}
