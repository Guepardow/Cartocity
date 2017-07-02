# = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 1er juillet 2017
# Projet - Dataviz d'une ville
#
# Mehdi Miah
#
# BUG : Abtention (s) et majuscule    
# = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == Préambule ======================================

rm(list = ls())
cat("\014")

setwd("C:/Users/Mehdi/Documents/Research/Présidentielles 2017 Montreuil")

library(dplyr)
library(data.table)
library(tidyr)

# == Ouverture des fichiers ======================================

# Fichier contenant les votes
vote_pr2017_tour1 = fread("./data/vote/output/2017_presidentielles_montreuil.csv", 
                        data.table = FALSE)

# == Transformation en table pivot ===============================

df = vote_pr2017_tour1 %>% spread(choix, vote)

# == Calcul de statistiques ======================================

# Taux d'abstention par bureau de vote
taux_abstention = vote_pr2017_tour1 %>%
  group_by(num_bureau) %>%
  summarise(total = sum(vote)) %>%
  merge(df) %>%
  select(num_bureau, Abtention, total) %>%
  mutate(taux_abstention = round((Abtention)/total * 100, 2))

# == Sauvegarde ==================================================

write.table(taux_abstention, "./data/vote/output/taux_abstention.csv", row.names = FALSE, quote = FALSE, sep = ";")


