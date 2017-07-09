get_max_voix = function(election_tour){
  # (input) election_tour : data.frame contenant les voix pour des candidats
  #
  # (output)              : le score le plus élevé obtenu par un candidat
  #
  # Remarque : le pourcentage de tous les choix est calculé par choix/(somme de tous les voix).
  
  # votes favorables
  df = election_tour %>%
    spread(choix, vote) %>%
    select(-num_bureau)
  
  df$pct_max = apply(df, 1, function(x) max(x))
  df$inscrits = apply(df, 1, function(x) {sum(x)-x["pct_max"]})
  df$voix = df$pct_max / df$inscrits * 100
  
  return(max(df$voix))
  
}