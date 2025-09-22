
rm(list=ls())
source("librairies.R")
source("fonctions.R")

#PARAMETRES : 
repertoire_fichiers_txt = "D:/alignements-main/ANALYSE/matrice"   # > tableau_counts.txt
tableau_counts = "counts_paired_zhang_2022_bulk"
chemin_repertoire_output = "D:/alignements-main/ANALYSE/matrice"   # > counts_names.txt  et genes.txt
counts = function_recupere_tab_counts_txt(tableau_counts)       
noms_genes = "genes"    

#ARCHITECTURE SIMPLE DU SCRIPT : 
longueurs_genes = recupere_longueurs_genes(counts)
counts = function_supprimer_x_premieres_colonnes(counts,5)
counts = function_change_NOMS_ALIGNEMENTS(counts,tableau_counts)
counts_norm = NORMALISATION(counts = counts,tableau_counts = tableau_counts ,normalisation = "vst" ,filename = "plot.png" ,longueurs_genes = longueurs_genes)
counts_norm_genes = function_selection_gene_dans_counts_AND_CONVERSION_NOM_DES_GENES(noms_genes = noms_genes,counts = counts_norm)
function_pheatmap(counts =counts_norm_genes ,plot_titre ="titre.png" ,filename = "titre.png")

















