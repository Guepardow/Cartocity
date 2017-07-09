# = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 9 juillet 2017
# Projet - Dataviz d'une ville
#
# Mehdi Miah
#
# BUG :  
# = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == Préambule ======================================

rm(list = ls())
cat("\014")

setwd("C:/Users/Mehdi/Desktop/Cartocity/")

library(dplyr)
library(data.table)
library(tidyr)

# == Ouverture des fichiers ======================================

# Fichier contenant les votes
vote_pr2017_tour2 = fread("./data/vote/2017 - Presidentielles/2nd tour/intermediaire/montreuil.csv", 
                          data.table = FALSE)

# == Formatage ===============================

df = vote_pr2017_tour2 %>% 
  select(-Inscrits, -Votants, -Exprimes) %>%
  rename(blanc = Blancs) %>%
  rename(nul = Nuls) %>%
  rename(abstention = Abstention) %>%
  rename(num_bureau = Bureau) %>%
  gather(choix, vote, -num_bureau)

# == Sauvegarde ==================================================

write.table(df, "./data/vote/2017 - Presidentielles/2nd tour/output/montreuil.csv", row.names = FALSE, quote = FALSE, sep = ";")


