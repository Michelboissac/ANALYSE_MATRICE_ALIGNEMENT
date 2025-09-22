# 2.2) NORMALISATION 
#NORMALISATION de counts (a faire sur le plus grands jeux de donnée, tableau entiers avec 12000 genes)
print("normalisation :")
counts_norm = NORMALISATION(counts = counts,tableau_counts = tableau_counts,normalisation = normalisation,filename=paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,"_dispEsts.png"),liste_conditions_a_garder_etoile=liste_conditions_a_garder_etoile,longueurs_genes=longueurs_genes)
#SAUVEGARDE DU TABLEAU COUNTS Normalisé
#sauvegarde le tableau counts normalise
head(counts_norm)
write.table(counts_norm,paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,".","all_genes.counts.txt"),row.names=TRUE,col.names = TRUE, quote = FALSE, sep = "\t")


# 2.2) SELECTION GENES
#selectionne une sous partie du tableau normalise avec les genes d'interets 
counts_norm_genes = function_selection_gene_dans_counts_AND_CONVERSION_NOM_DES_GENES(noms_genes,counts_norm)
#selectionne une sous partie du tableau normalise avec les genes d'interets 
counts_genes = function_selection_gene_dans_counts_AND_CONVERSION_NOM_DES_GENES(noms_genes,counts)
#sauvegarde le sous tableau normalise avec nos genes
write.table(counts_norm_genes,paste0(chemin_repertoire_output,"/",nom_repertoire_output,".",normalisation,".",noms_genes,".counts.txt"),row.names=TRUE,col.names = TRUE, quote = FALSE, sep = "\t")

