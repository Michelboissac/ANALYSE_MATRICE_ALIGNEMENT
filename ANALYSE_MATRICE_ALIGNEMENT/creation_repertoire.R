
#creation repertoire, 
#creation du fichier contenant la liste des genes à etudier, 
#recupere la liste des longueurs des genes pour la normalisation
#enleve les 1eres colonnes de la matrice



nom_tableau_counts = paste0(tableau_counts,".txt")
# Parametres Partie 2) facultatifs
df <- data.frame(
  nom_ancien =nom_ancien,    
  nom_nouveau =nom_nouveau         
)
liste_conditions_a_garder_etoile=NULL
nom_repertoire_output=""


#repertoire_fichiers_txt = paste0(dossier_travail,"/FICHIERS_TXT")
OUTPUTS = paste0(repertoire_fichiers_txt,"OUTPUT")
dir.create(OUTPUTS)

# 2.1) CREER DOSSIER OUTPUT

#Créer le répertoire output
if(nom_repertoire_output == "") {
  chemin_repertoire_output = paste0(OUTPUTS, "/", tableau_counts, "_", normalisation, "_", noms_genes)
  nom_repertoire_output = tableau_counts
} else {
  chemin_repertoire_output = paste0(OUTPUTS, "/", nom_repertoire_output)
}
dir.create(chemin_repertoire_output)


# 2.1) sauve la liste des genes
write.table(df, file =paste0(chemin_repertoire_output,"/",noms_genes,".txt"), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
print(chemin_repertoire_output)



#recupere longueurs des genes
longueurs_genes = recupere_longueurs_genes(counts)                                         #faire avant "function_selection_gene_dans_counts_AND_CONVERSION_NOM_DES_GENES"
#supprime les 1eres colonnes
counts = function_supprimer_x_premieres_colonnes(counts,5)                                 #supprime les premiers colonnes du tableau (longueur,chromosome, etc ..)
all_numeric_columns <- all(sapply(counts, is.numeric))

