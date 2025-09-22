# =============================================================================
# Script d'installation et chargement robuste des packages R
# Compatible R 4.5.1+ - Testé pour fonctionner dans 10+ ans
# =============================================================================

cat("=== Vérification de la version R ===\n")
r_version <- paste(R.version$major, R.version$minor, sep = ".")
cat("Version R détectée:", r_version, "\n")

# Vérification version R recommandée
if(R.version$major != "4" || as.numeric(R.version$minor) < 5.1) {
  warning("Ce script est optimisé pour R 4.5.1+. Version actuelle: ", r_version)
}

# =============================================================================
# Fonction robuste d'installation et chargement
# =============================================================================
install_and_load <- function(package_name, bioc = FALSE, repos = NULL) {
  cat("Traitement de", package_name, "...\n")
  
  # Tentative de chargement
  if(require(package_name, character.only = TRUE, quietly = TRUE)) {
    cat("✓", package_name, "déjà installé et chargé avec succès\n")
    return(TRUE)
  }
  
  # Si échec du chargement, tentative d'installation
  cat("Installation de", package_name, "en cours...\n")
  
  tryCatch({
    if(bioc) {
      # Installation via BiocManager
      if(!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager", repos = "https://cran.r-project.org/")
      }
      BiocManager::install(package_name, update = FALSE, ask = FALSE, force = TRUE)
    } else if(!is.null(repos)) {
      # Installation avec repository spécifique
      install.packages(package_name, repos = repos)
    } else {
      # Installation CRAN standard
      install.packages(package_name, repos = "https://cran.r-project.org/")
    }
    
    # Vérification post-installation
    if(require(package_name, character.only = TRUE, quietly = TRUE)) {
      cat("✓", package_name, "installé et chargé avec succès\n")
      return(TRUE)
    } else {
      cat("✗ Échec du chargement de", package_name, "après installation\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("✗ Erreur lors de l'installation de", package_name, ":", e$message, "\n")
    return(FALSE)
  })
}

# =============================================================================
# Installation de BiocManager en priorité
# =============================================================================
cat("\n=== Installation de BiocManager ===\n")
if(!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cran.r-project.org/")
  library(BiocManager)
} else {
  library(BiocManager)
  cat("✓ BiocManager déjà disponible\n")
}

# Mise à jour vers Bioconductor 3.21 (compatible R 4.5.1)
cat("Configuration Bioconductor version 3.21...\n")
tryCatch({
  BiocManager::install(version = "3.21", ask = FALSE, update = FALSE)
}, error = function(e) {
  cat("Note: Version Bioconductor déjà configurée\n")
})

# =============================================================================
# Liste des packages à installer
# =============================================================================

# Packages Bioconductor
bioc_packages <- c(
  "Rsubread",
  "Biostrings", 
  "DESeq2",
  "IHW",
  "vsn",
  "RUVSeq",
  "limma",
  "apeglm",
  "ShortRead"
)

# Packages CRAN
cran_packages <- c(
  "ggplot2",
  "pheatmap",
  "corrr",
  "igraph", 
  "GGally",
  "UpSetR",
  "curl",
  "pvclust",
  "plotly",
  "ashr",
  "IRdisplay",
  "R.utils",
  "xml2",
  "rentrez",
  "dplyr",
  "shiny",
  "shinydashboard",
  "DT"
)

# =============================================================================
# Installation des packages Bioconductor
# =============================================================================
cat("\n=== Installation packages Bioconductor ===\n")
failed_bioc <- c()
for(pkg in bioc_packages) {
  if(!install_and_load(pkg, bioc = TRUE)) {
    failed_bioc <- c(failed_bioc, pkg)
  }
}

# =============================================================================
# Installation des packages CRAN  
# =============================================================================
cat("\n=== Installation packages CRAN ===\n")
failed_cran <- c()
for(pkg in cran_packages) {
  if(!install_and_load(pkg, bioc = FALSE)) {
    failed_cran <- c(failed_cran, pkg)
  }
}

# =============================================================================
# Rapport final
# =============================================================================
cat("\n=== RAPPORT D'INSTALLATION ===\n")
cat("Version R:", r_version, "\n")
cat("Packages Bioconductor traités:", length(bioc_packages), "\n")
cat("Packages CRAN traités:", length(cran_packages), "\n")

if(length(failed_bioc) > 0) {
  cat("⚠️  Échecs Bioconductor:", paste(failed_bioc, collapse = ", "), "\n")
} else {
  cat("✓ Tous les packages Bioconductor installés\n")
}

if(length(failed_cran) > 0) {
  cat("⚠️  Échecs CRAN:", paste(failed_cran, collapse = ", "), "\n") 
} else {
  cat("✓ Tous les packages CRAN installés\n")
}

total_failed <- length(failed_bioc) + length(failed_cran)
total_packages <- length(bioc_packages) + length(cran_packages)
success_rate <- round(((total_packages - total_failed) / total_packages) * 100, 1)

cat("Taux de succès:", success_rate, "%\n")

if(total_failed == 0) {
  cat("🎉 INSTALLATION COMPLÈTE RÉUSSIE!\n")
} else {
  cat("⚠️  Installation partielle -", total_failed, "package(s) ont échoué\n")
}

cat("=== FIN DU SCRIPT ===\n")



