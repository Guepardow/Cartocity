get_voix_bureau = function(election_tour, mon_bureau, version_calcul = "Officielle"){
  # (input) election_tour   : data.frame contenant les voix pour des candidats
  # (input) mon_bureau      : un des bureaux de vote
  # (input) version_calcul  : version de calcul ('Officielle' ou 'Brute')
  #
  # (output)                : un data.frame donnant la correspondance entre les candidats
  #                           et leur pourcentage de voix calculé soit de manière brute, soit à 
  #                           partir des formules du Ministère de l'Intérieur dans un bureau de vote donné
  #
  # Remarque : pour la version 'Officielle', le pourcentage de l'abstention est calculé par absention/(somme de tous les voix), 
  # pour les votes 'blanc' et 'nul' , le pourcentage est calculé par choix/(somme de tous les voix - abstention)
  # et pour les candidats physiques, la formule est choix/(somme de tous les voix - abstention - blanc - nul)
  
  # Filtrage des candidats en fonction de la version de calcul
  if(version_calcul == "Officielle"){
    election_tour = election_tour %>% 
      filter(!choix %in% c("abstention", "nul", "blanc") )
  }
  
  # votes favorables
  favorable = election_tour %>% 
    filter(num_bureau == mon_bureau)
  
  # votes total
  total = election_tour %>%
    filter(num_bureau == mon_bureau) %>%
    select(vote) %>%
    sum() 
  
  # pourcentage de voix
  final = favorable %>%
    mutate(pct = round(vote/total * 100,2)) %>%
    select(num_bureau, choix, pct)
  
  return(final)
  
}

# == Tests unitaires ==
if(FALSE){
  election_tour = fread("./data/vote/2017 - Presidentielles/1er tour/output/montreuil.csv", 
                        data.table = FALSE, encoding = "UTF-8")
  
  #Test 1 : version Officielle
  voix = get_voix_bureau(election_tour, "54", version_calcul = "Officielle")
  
  #Test 2 : version Brute
  voix = get_voix_bureau(election_tour, "28", version_calcul = "Brute")

  
}

