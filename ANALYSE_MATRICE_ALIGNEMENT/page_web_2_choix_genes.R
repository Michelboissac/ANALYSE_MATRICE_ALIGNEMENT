library(shiny)

ui <- fluidPage(
  titlePanel("🧬 Gestion des Noms de Gènes"),
  
  tags$head(
    tags$style(HTML("
      .form-control { font-size: 14px; height: 40px; }
      textarea.form-control { height: 80px !important; resize: vertical; }
      .help-text { color: #666; font-size: 12px; margin-bottom: 15px; }
      .config-section {
        background-color: #f8f9fa; padding: 20px; border-radius: 8px;
        margin-bottom: 25px; border: 1px solid #dee2e6;
      }
      .section-title {
        color: #495057; margin-bottom: 20px; border-bottom: 2px solid #007bff;
        padding-bottom: 10px;
      }
      .center-button { text-align: center; margin: 40px 0; padding: 20px; }
      .search-section {
        background-color: #f1f3f4; padding: 20px; border-radius: 8px;
        margin-top: 30px; border: 1px solid #dee2e6;
      }
      .search-input-group { display: flex; align-items: center; gap: 10px; margin-bottom: 20px; }
      .search-input { flex: 1; }
      .search-button { min-width: 120px; }
      .genes-display { display: flex; gap: 20px; }
      .genes-list, .search-results {
        flex: 1; background-color: white; padding: 15px; border-radius: 5px;
        border: 1px solid #ccc;
      }
      .search-results { background-color: #e8f5e8; border: 1px solid #28a745; }
    "))
  ),
  
  # ✅ AJOUTÉ - Retour au menu principal
  fluidRow(
    column(12,
           actionButton("retour_menu", "← Retour au menu principal", 
                        class = "btn btn-secondary"),
           hr()
    )
  ),
  
  # Section noms anciens/nouveaux + noms_genes
  div(class = "config-section",
      h3(class = "section-title", "🧬 Configuration des Noms de Gènes"),
      
      fluidRow(
        column(6,
               h4("Nom Ancien"),
               textAreaInput("nom_ancien", label = NULL, value = "",
                             placeholder = "Exemple: LOC406124 Grd LCCH3 Amel_8916 GluCl",
                             width = "100%"),
               div(class = "help-text", "Noms actuels des gènes à modifier")
        ),
        column(6,
               h4("Nom Nouveau"), 
               textAreaInput("nom_nouveau", label = NULL, value = "",
                             placeholder = "Exemple: LOC406124_Rdl Grd LCCH3 Amel_8916 GluCl", 
                             width = "100%"),
               div(class = "help-text", "Nouveaux noms pour la conversion")
        )
      ),
      
      # Ajout de noms_genes (unique valeur)
      fluidRow(
        column(12,
               h4("Nom de la variable noms_genes"),
               textInput("noms_genes", label = NULL, value = "",
                         placeholder = "Exemple: liste_de_genes"),
               div(class = "help-text", "Nom de la variable contenant les données de gènes (une seule valeur)")
        )
      )
  ),
  
  # Bouton de validation
  div(class = "center-button",
      actionButton("valider", "✅ Valider et Continuer", 
                   class = "btn-primary btn-lg", 
                   style = "padding: 20px 40px; font-size: 20px; font-weight: bold;")
  ),
  
  # Section recherche
  div(class = "search-section",
      h3(class = "section-title", "🔎 Recherche dans la Liste des Gènes"),
      
      div(class = "search-input-group",
          div(class = "search-input",
              textInput("search_gene", "Rechercher un gène :", "", width = "100%")),
          div(class = "search-button",
              actionButton("bouton_rechercher", "🔍 Rechercher", 
                           class = "btn-info", style = "height: 40px; width: 100%;"))
      ),
      
      div(class = "genes-display",
          div(class = "genes-list",
              h4("📋 Liste Complète des Gènes"),
              div(style = "max-height: 400px; overflow-y: auto;",
                  tableOutput("all_genes")
              )
          ),
          div(class = "search-results",
              h4("🎯 Résultats de Recherche"),
              div(style = "max-height: 400px; overflow-y: auto;",
                  tableOutput("search_results")
              )
          )
      )
  )
)

server <- function(input, output, session) {
  
  # ✅ AJOUTÉ - Bouton retour au menu
  observeEvent(input$retour_menu, {
    stopApp(returnValue = "menu")
  })
  
  # Liste complète (soit counts, soit exemple)
  genes <- reactive({
    if (exists("counts", envir = .GlobalEnv)) {
      rownames(counts)
    } else {
      c("Gene1","Gene2","Gene3")
    }
  })
  
  # Affichage liste complète
  output$all_genes <- renderTable({
    data.frame(Gene = genes(), stringsAsFactors = FALSE)
  }, rownames = FALSE)
  
  # Résultats recherche
  search_results <- reactiveVal(data.frame(Gene = character(0), stringsAsFactors = FALSE))
  output$search_results <- renderTable({ search_results() }, rownames = FALSE)
  
  observeEvent(input$bouton_rechercher, {
    if (input$search_gene != "") {
      filtered <- genes()[grepl(input$search_gene, genes(), ignore.case = TRUE)]
      if (length(filtered) > 0) {
        search_results(data.frame(Gene = filtered, stringsAsFactors = FALSE))
        showNotification(paste("Trouvé", length(filtered), "gène(s)"), type = "message", duration = 3)
      } else {
        search_results(data.frame(Gene = "Aucun résultat trouvé", stringsAsFactors = FALSE))
        showNotification("Aucun gène trouvé", type = "warning", duration = 3)
      }
    } else {
      search_results(data.frame(Gene = character(0), stringsAsFactors = FALSE))
      showNotification("Veuillez saisir un terme de recherche", type = "warning", duration = 2)
    }
  })
  
  # ✅ CORRIGÉ - Validation et retour au menu
  observeEvent(input$valider, {
    noms_anciens  <- if (trimws(input$nom_ancien)  != "") unlist(strsplit(trimws(input$nom_ancien),  "\\s+")) else character(0)
    noms_nouveaux <- if (trimws(input$nom_nouveau) != "") unlist(strsplit(trimws(input$nom_nouveau), "\\s+")) else character(0)
    noms_genes_in <- trimws(input$noms_genes)
    
    nom_ancien  <<- noms_anciens
    nom_nouveau <<- noms_nouveaux
    noms_genes  <<- noms_genes_in
    
    showNotification(paste("✅ Variables créées : nom_ancien, nom_nouveau, noms_genes =", noms_genes_in),
                     type = "message", duration = 3)
    
    # ✅ CORRIGÉ - Retour au menu au lieu de juste stopApp()
    stopApp(returnValue = "menu")  # ou "analyses" selon votre logique
  })
}

# ✅ SUPPRIMÉ - Ne pas lancer directement
# Géré par le lanceur principal

# Les lignes suivantes sont supprimées car gérées par le lanceur :
# cat("🚀 Lancement de l'interface de gestion des noms de gènes...\n")
# resultats <- runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)