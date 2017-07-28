get_max_voix = function(election_tour, version_calcul = "Officielle"){
  # (input) election_tour : data.frame contenant les voix pour des candidats
  # (input) version_calcul        : mode de calcul ('Officielle' ou 'Brute')
  #
  # (output)              : le score le plus élevé obtenu par un candidat dans un bureau de vote
  #
  # Remarque : le pourcentage de tous les choix est calculé par choix/(somme de tous les voix) dans le cas brute
  #         : le pourcentage reprend les calculs officiels dans l'autre cas.
  
  # votes favorables
  df = election_tour %>%
    spread(choix, vote) %>%
    select(-num_bureau)
  
  # Calcul du nombre de voix maximales selon le type de calcul
  if(version_calcul == 'Brute'){
    
    df$pct_max = apply(df, 1, function(x) max(x))
    df$inscrits = apply(df, 1, function(x) {sum(x)-x["pct_max"]})
    df$voix = df$pct_max / df$inscrits * 100
    
  }else if(version_calcul == 'Officielle'){
    
    # délimitation des choix
    choix = names(df)
    candidats_physiques = setdiff(choix, c("abstention", "blanc", "nul"))
    
    # calcul max des candidats physiques
    df$max_candidat_physique = apply(df[, candidats_physiques], 1, max)
    
    # calcul des totaux
    df$total = rowSums(df[, choix])
    df$total_hors_abstention = df$total - df$abstention
    df$total_candidats_physiques = df$total_hors_abstention - df$blanc - df$nul
  
    # calcul des pourcentages
    df$taux_abstention = df$abstention / df$total * 100
    df$taux_blanc = df$blanc / df$total_hors_abstention * 100
    df$taux_nul = df$nul / df$total_hors_abstention * 100
    df$taux_max_candidat_physique = df$max_candidat_physique / df$total_candidats_physiques * 100
    
    # agrége les données
    df$voix = apply(df[, c("taux_abstention", "taux_blanc", "taux_nul", "taux_max_candidat_physique")], 1, max)
    
  }
  
  return(max(df$voix))
  
}

if(FALSE){
  election_tour = fread("./data/vote/2017 - Presidentielles/1er tour/output/montreuil.csv", data.table = FALSE, encoding = "UTF-8")

  res = get_max_voix(election_tour, version_calcul = "Officielle")
}