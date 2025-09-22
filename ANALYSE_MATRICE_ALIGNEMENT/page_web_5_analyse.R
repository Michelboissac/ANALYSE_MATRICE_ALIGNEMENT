# =============================================================================
# Interface Shiny pour analyses RNA-seq
# Boutons interactifs pour chaque type d'analyse
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)

ui <- fluidPage(
      fluidRow(
      column(12,
             actionButton("retour_menu", "← Retour au menu principal", 
                          class = "btn btn-secondary"),
             hr()
      )
    ),
    
    
    # Section Heatmaps
    fluidRow(
      column(12,
             div(class = "analysis-section",
                 h3("🔥 Analyses Heatmap", style = "color: #e74c3c;"),
                 fluidRow(
                   column(6,
                          actionButton("heatmap_norm", 
                                       "Heatmap Normalisée", 
                                       class = "btn btn-danger btn-analysis",
                                       icon = icon("fire"))
                   ),
                   column(6,
                          actionButton("heatmap_raw", 
                                       "Heatmap Raw", 
                                       class = "btn btn-warning btn-analysis",
                                       icon = icon("chart-area"))
                   )
                 ),
                 fluidRow(
                   column(6,
                          actionButton("pheatmap_norm", 
                                       "PHeatmap Normalisée", 
                                       class = "btn btn-danger btn-analysis",
                                       icon = icon("th"))
                   ),
                   column(6,
                          actionButton("pheatmap_raw", 
                                       "PHeatmap Raw", 
                                       class = "btn btn-warning btn-analysis",
                                       icon = icon("border-all"))
                   )
                 )
             )
      )
    ),
    
    # Section Boxplots et Barplots
    fluidRow(
      column(12,
             div(class = "analysis-section",
                 h3("📊 Analyses Boxplot & Barplot", style = "color: #3498db;"),
                 fluidRow(
                   column(6,
                          actionButton("boxplot_raw", 
                                       "Boxplot Reads Raw", 
                                       class = "btn btn-primary btn-analysis",
                                       icon = icon("chart-simple"))
                   ),
                   column(6,
                          actionButton("boxplot_norm", 
                                       "Boxplot Reads Normalisées", 
                                       class = "btn btn-info btn-analysis",
                                       icon = icon("chart-line"))
                   )
                 ),
                 fluidRow(
                   column(4,
                          actionButton("barplot_reads_gene", 
                                       "Barplot Reads/Gène", 
                                       class = "btn btn-primary btn-analysis",
                                       icon = icon("chart-column"))
                   ),
                   column(4,
                          actionButton("barplot_genes_expr", 
                                       "Barplot Gènes Exprimés", 
                                       class = "btn btn-info btn-analysis",
                                       icon = icon("dna"))
                   ),
                   column(4,
                          actionButton("barplot_genome_expr", 
                                       "Barplot Expression Génome", 
                                       class = "btn btn-primary btn-analysis",
                                       icon = icon("globe"))
                   )
                 )
             )
      )
    ),
    
    # Section Analyses multivariées
    fluidRow(
      column(12,
             div(class = "analysis-section",
                 h3("📈 Analyses Multivariées", style = "color: #27ae60;"),
                 fluidRow(
                   column(4,
                          actionButton("genes_communs", 
                                       "Gènes Communs (Venn)", 
                                       class = "btn btn-success btn-analysis",
                                       icon = icon("circle-nodes"))
                   ),
                   column(4,
                          actionButton("acp_2d", 
                                       "ACP 2D", 
                                       class = "btn btn-success btn-analysis",
                                       icon = icon("project-diagram"))
                   ),
                   column(4,
                          actionButton("acp_3d", 
                                       "ACP 3D", 
                                       class = "btn btn-success btn-analysis",
                                       icon = icon("cube"))
                   )
                 ),
                 fluidRow(
                   column(6,
                          actionButton("clustering", 
                                       "Clustering Hiérarchique", 
                                       class = "btn btn-success btn-analysis",
                                       icon = icon("sitemap"))
                   )
                 )
             )
      )
    
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$retour_menu, {
    stopApp(returnValue = "menu")
  })
  


  # Heatmaps
  observeEvent(input$heatmap_norm, {
    tryCatch({
      function_heatmap(counts = counts_norm_genes,
                       plot_titre = paste0(nom_repertoire_output,".",normalisation,".",noms_genes),
                       filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,".",noms_genes,".heatmap.png"))
    })
  })
  
  observeEvent(input$heatmap_raw, {
    tryCatch({
      function_heatmap(counts = counts_genes,
                       plot_titre = paste0(nom_repertoire_output,".",noms_genes),
                       filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",noms_genes,".heatmap.png"))
    })
  })
  
  observeEvent(input$pheatmap_norm, {
    tryCatch({
      counts_norm_genes_sorted <- counts_norm_genes[order(rownames(counts_norm_genes)), order(colnames(counts_norm_genes))]
      function_pheatmap(counts = counts_norm_genes_sorted,
                        plot_titre = paste0(nom_repertoire_output,".",normalisation,".",noms_genes),
                        filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,".",noms_genes,".pheatmap.png"))
    })
  })
  
  observeEvent(input$pheatmap_raw, {
    tryCatch({
      counts_genes_sorted <- counts_genes[order(rownames(counts_genes)), order(colnames(counts_genes))]
      function_pheatmap(counts = counts_genes_sorted,
                        plot_titre = paste0(nom_repertoire_output,".",noms_genes),
                        filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",noms_genes,".pheatmap.png"))
      })
  })
  
  # Boxplots
  observeEvent(input$boxplot_raw, {
    tryCatch({
      function_boxplot_nbr_reads(counts,
                                 plot_titre = nom_repertoire_output,
                                 filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".boxplot_nbr_reads.png"))
    })
  })
  
  observeEvent(input$boxplot_norm, {
    tryCatch({
      function_boxplot_nbr_reads(counts_norm,
                                 plot_titre = nom_repertoire_output,
                                 filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,".boxplot_nbr_reads.png"))
    })
  })
  
  # Barplots
  observeEvent(input$barplot_reads_gene, {
    tryCatch({
      function_barplot_nbr_reads_moyen_par_gene_exprime(counts = counts_norm,
                                                        filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,"_all_genes.barplot_nbr_reads_moyen_par_gene_exprime"))
    })
  })
  
  observeEvent(input$barplot_genes_expr, {
    tryCatch({
      function_barplot_nbr_genes_expr(counts = counts,
                                      filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,"_all_genes.barplot_nbr_genes_expr"))
    })
  })
  
  observeEvent(input$barplot_genome_expr, {
    tryCatch({
      dir.create(paste0(chemin_repertoire_output,"/barplot_genome_expression_",normalisation), showWarnings = FALSE)
      function_barplot_genome_expression(counts = counts_norm,
                                         filename = paste0(chemin_repertoire_output,"/barplot_genome_expression_",normalisation,"/",nom_repertoire_output,".",normalisation,"_barplot_genome_expression"))
    })
  })
  
  # Analyses multivariées
  observeEvent(input$genes_communs, {
    tryCatch({
      function_genes_communs(counts)
    })
  })
  
  observeEvent(input$acp_2d, {
    tryCatch({
      function_ACP(counts = counts_norm,
                   plot_titre = paste0(nom_repertoire_output,".",normalisation,"_all_genes"), 
                   filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,"_all_genes.acp.png"))
    })
  })
  
  observeEvent(input$acp_3d, {
    tryCatch({
      function_3D_PCA(counts = counts_norm,
                      filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,"_all_genes.acp3D.html"))
    })
  })
  
  observeEvent(input$clustering, {
    tryCatch({
      function_cluster_hierarchique(counts = counts_norm,
                                    filename = paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,"_all_genes.dendrogramme"))
    })
  })
  
}
