
tableau_counts_names = paste0(tableau_counts,"_names.txt")
file = paste0(chemin_repertoire_output,"/",tableau_counts_names)

df <- data.frame(
  nom_actuel =  colnames(counts), 
  nom_nouveau = resultats$Saisi
)


if (!is.null(chemin_vers_les_nouveaux_noms_txt)){
  file.copy(chemin_vers_les_nouveaux_noms_txt,
            file)
}


if (!file.exists(file)){
  write.table(df, file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}

counts = function_change_NOMS_ALIGNEMENTS(counts,tableau_counts)                           #change le nom des alignements (colonnes) par cexu du fichiers *names.txt
#alignements que l'on veut garder

counts = function_conditions_a_garder_dans_counts(counts,liste_conditions_a_garder_etoile) #pour recuperer uniquement certains alignements (colonnes)

