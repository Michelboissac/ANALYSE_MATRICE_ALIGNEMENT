# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("Pipeline Bioinfo"),
  fluidRow(
    column(3,
           actionButton("btn_matrice", "1 : Matrice d'alignement", width = "100%", 
                        class = "btn btn-primary btn-lg"),

           br(), br(),
           actionButton("btn_gene", "2 : Sélection de gènes", width = "100%",
                        class = "btn btn-primary btn-lg"),
           br(), br(),
           actionButton("btn_norm", "3 : normalisation", width = "100%",
                        class = "btn btn-primary btn-lg"),
           br(), br(),
           actionButton("btn_noms_alignements", "4 : noms alignements", width = "100%", 
                        class = "btn btn-primary btn-lg"),
           br(), br(), br(), br(),
           actionButton("btn_analyse", "Analyses", width = "100%",
                        class = "btn btn-warning btn-lg"),
           br(), br(),
           actionButton("btn_expr_diff", "expression differentielle", width = "100%",
                        class = "btn btn-warning btn-lg")
    ),
    column(9,
           h3("Logs d'exécution:"),
           verbatimTextOutput("log")
    )
  )
)

server <- function(input, output, session) {
  logText <- reactiveVal("Application démarrée...\n")
  
  observeEvent(input$btn_matrice, {
    logText(paste(logText(), "Lancement de la matrice d'alignement...\n"))
    stopApp(returnValue = "matrice")
  })
  
  observeEvent(input$btn_noms_alignements, {
    logText(paste(logText(), "changement des noms d'alignement...\n"))
    stopApp(returnValue = "noms_alignements")
  })
  
  observeEvent(input$btn_gene, {
    logText(paste(logText(), "Lancement de la sélection de gènes...\n"))
    stopApp(returnValue = "genes")
  })
  
  observeEvent(input$btn_norm, {
    logText(paste(logText(), "Lancement de la normalisation...\n"))
    stopApp(returnValue = "normalisation")
  })
  
  observeEvent(input$btn_analyse, {
    logText(paste(logText(), "Lancement des analyses...\n"))
    stopApp(returnValue = "analyses")
  })
  
  observeEvent(input$btn_expr_diff, {
    logText(paste(logText(), "Lancement des analyses...\n"))
    stopApp(returnValue = "expr_diff")
  })
  
  output$log <- renderText({
    logText()
  })
}

# Ne pas lancer directement - sera géré par le lanceur