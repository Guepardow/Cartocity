get_voix_brute = function(election_tour, mon_choix){
  # (input) election_tour : data.frame contenant les voix pour des candidats
  # (input) mon_choix     : un des candidat
  #
  # (output)              : un data.frame donnant la correspondance entre les bureaux de vote
  #                         et son pourcentage réel de voix
  #
  # Remarque : le pourcentage de tous les choix est calculé par choix/(somme de tous les voix).
  
  # votes favorables
  favorable = election_tour %>% 
    filter(choix == mon_choix)
  
  # votes total
    total = election_tour %>%
      group_by(num_bureau) %>%
      summarise(count = sum(vote))
  
  # pourcentage de voix
  final = favorable %>%
    left_join(total, by = "num_bureau") %>%
    mutate(pct = round(vote/count * 100,2)) %>%
    select(num_bureau, choix, pct)
  
  return(final)
  
}

# == Tests unitaires ==
if(FALSE){
  election_tour = fread("./data/vote/2017 - Presidentielles/1er tour/output/montreuil.csv", data.table = FALSE, encoding = "UTF-8")
  
  #Test 1 : abstention
  voix = get_voix_brute(election_tour, "abstention")
  
  #Test 2 : candidat
  voix = get_voix_brute(election_tour, "Benoit HAMON")
  
  #Test 3 : blanc
  voix = get_voix_brute(election_tour, "blanc")
  
  #Test 4 : nul
  voix = get_voix_brute(election_tour, "nul")
  
  
}

