get_voix = function(election_tour, mon_choix){
  # (input) election_tour : data.frame contenant les voix pour des candidats
  # (input) mon_choix     : un des candidat
  #
  # (output)              : un data.frame donnant la correspondance entre les bureaux de vote
  #                         et son pourcentage de voix
  #
  # Remarque : le pourcentage de l'abstention est calculé par absention/(somme de tous les voix), 
  # pour les votes 'blanc' et 'nul' , le pourcentage est calculé par choix/(somme de tous les voix - abstention)
  # et pour les candidats physiques, la formule est choix/(somme de tous les voix - abstention - blanc - nul)
  
  # votes favorables
  favorable = election_tour %>% 
    filter(choix == mon_choix)
  
  # votes total
  # il faut faire la distintion si le choix est l'abstention, blanc, nul ou autre
  if(mon_choix == "abstention"){
  total = election_tour %>%
    group_by(num_bureau) %>%
    summarise(count = sum(vote))
  }else if(mon_choix == "blanc" | mon_choix == "nul"){# il s'agit d'un vote blanc ou nul
    total = election_tour %>%
      filter(!choix %in% c("abstention")) %>%
      group_by(num_bureau) %>%
      summarise(count = sum(vote))
  }else{ #il s'agit d'un candidat
    total = election_tour %>%
      filter(!choix %in% c("abstention", "blanc", "nul")) %>%
      group_by(num_bureau) %>%
      summarise(count = sum(vote))
  }
    
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
  voix = get_voix(election_tour, "abstention")
  voix_verif1 = data.frame(num_bureau = 1:57)
  
  #Test 2 : candidat
  voix = get_voix(election_tour, "Benoit HAMON")
  
  #Test 3 : blanc
  voix = get_voix(election_tour, "blanc")
  
  #Test 4 : nul
  voix = get_voix(election_tour, "nul")
  
  
  }

