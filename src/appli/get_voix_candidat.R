get_voix_candidat = function(election_tour, mon_choix, version_calcul = "Officielle"){
  # (input) election_tour   : data.frame contenant les voix pour des candidats
  # (input) mon_choix       : un des candidat
  # (input) version_calcul  : mode de calcul ('Officielle' ou 'Brute')
  #
  # (output)              : un data.frame donnant la correspondance entre les bureaux de vote
  #                         et leur pourcentage de voix calculé soit de manière brute, soit à 
  #                        partir des formules du Ministère de l'Intérieur, pour un candidat donné
  #
  # Remarque : pour la version 'Officielle', le pourcentage de l'abstention est calculé par absention/(somme de tous les voix), 
  # pour les votes 'blanc' et 'nul' , le pourcentage est calculé par choix/(somme de tous les voix - abstention)
  # et pour les candidats physiques, la formule est choix/(somme de tous les voix - abstention - blanc - nul)
  
  # votes favorables
  favorable = election_tour %>% 
    filter(choix == mon_choix)
  
  # votes total
  if(version_calcul == "Officielle"){ # Cas en suivant les formules du Ministère
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
  }else if(version_calcul == "Brute"){ # Cas brute
    total = election_tour %>%
      group_by(num_bureau) %>%
      summarise(count = sum(vote))
  }
    
  # pourcentage de voix
  final = favorable %>%
    left_join(total, by = "num_bureau") %>%
    mutate(pct = round(vote/count * 100,2))
  
  return(final)
  
}

# == Tests unitaires ==
if(FALSE){
  
  setwd("C:/Users/Mehdi/Desktop/Cartocity")
  election_tour = fread("./data/vote/2017 - Presidentielles/1er tour/montreuil.csv", 
                        data.table = FALSE, encoding = "UTF-8")
  mon_choix = "abstention"
  
  #Test 1 : abstention
  voix = get_voix_candidat(election_tour, "abstention", version_calcul = "Officielle")

  #Test 2 : un candidat
  voix = get_voix_candidat(election_tour, "Benoit HAMON", version_calcul = "Officielle")
  
  #Test 3 : blanc
  voix = get_voix_candidat(election_tour, "blanc", version_calcul = "Officielle")
  
  #Test 4 : nul
  voix = get_voix_candidat(election_tour, "nul", version_calcul = "Officielle")
  
  }

