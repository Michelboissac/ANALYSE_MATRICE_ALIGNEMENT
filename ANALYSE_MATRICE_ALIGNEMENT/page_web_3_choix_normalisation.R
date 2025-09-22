library(shiny)

ui <- fluidPage(
  titlePanel("Normalisation"),
  
  fluidRow(
    column(12,
           selectInput("normalisation", 
                       label = "Type de normalisation :",
                       choices = list(
                         "Pas de normalisation" = "pas_de_normalisation",
                         "VST" = "vst",
                         "VST conditions" = "vst_conditions", 
                         "VST peu de gènes" = "vst_peu_de_genes",
                         "Log RPKM" = "log_rpkm",
                         "Log TPM" = "log_tpm",
                         "RLOG" = "rlog",
                         "Binaire" = "binaire"
                       ),
                       selected = "pas_de_normalisation", 
                       width = "100%")
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
  
  observeEvent(input$valider, {

    normalisation <<- input$normalisation
    
    Sys.sleep(1)
    
    stopApp(returnValue = "menu")  # Retour au menu
  })
}

