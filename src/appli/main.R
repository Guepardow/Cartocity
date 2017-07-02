# = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 1er juillet 2017
# Projet - Dataviz d'une ville
#
# Mehdi Miah
#
# BUG : encoding UTF-8 non active sur geojson_read
#     : bureaux_de_vote : code couleur de 17 sur la carte des bureaux 2015
#                       : nom 8 + 10 + 9 +
# = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == Préambule ======================================

rm(list = ls())
cat("\014")

setwd("C:/Users/Mehdi/Documents/Research/Cartocity")

library(dplyr)
library(data.table)
library(shiny)
library(leaflet)
library(geojsonio)

# == Ouverture des fichiers permettant la géolocalisation ===================

# Liste des élections disponibles : ensemble des dossiers présents dans ./data/vote
liste_elections = list.dirs(path = "./data/vote", full.names = FALSE, recursive = FALSE)

# carte des bureaux de vote
bureaux_de_vote = geojson_read("./data/bureaux_de_vote/intermediate/montreuil_57s.json", what = "sp")

# Ficher contenant le taux d'abstention
taux_abstention = fread("./data/vote/2017 - Présidentielles/1er tour/output/taux_abstention.csv", data.table = FALSE, colClasses = c())

# Rajout de l'abstention
bureaux_de_vote$abstention = sapply(bureaux_de_vote$num_bureau, function(num){taux_abstention$taux_abstention[taux_abstention$num_bureau == num]})

# == User interface ============================================

ui = fluidPage(
  
  #Titre
  titlePanel("Etude cartographique de Montreuil"),
  tabsetPanel(
    tabPanel(title = "Carte des élections", 
             
             column(width = 3,
                     selectInput(inputId = "election_choice",
                                 label = "Election",
                                 choices = liste_elections,
                                 selected = liste_elections[1])
                    ),
             
             column(width = 3,
             radioButtons(inputId = "tour_choice", label = "Choix du tour", choices = c("1er tour", "2nd tour"), 
                          selected = "1er tour")
             ),
             
             leafletOutput("mymap", height = 550) %>% print
    ),
    tabPanel(title = "Carte socio-économique"),
    tabPanel(title = "Analyse des bureaux"), 
    tabPanel(title = "Répartition des votes au second tour"),
    tabPanel(title = "Méthodologie")
  )
)

# == Server ====================================================

server = shinyServer(function(input, output, session) {

  # == Colorimétrie ==
  
  pal <- colorNumeric(palette = "Reds", domain = bureaux_de_vote$abstention)
  
  # == Nom des polygones ==
  
  popup <- paste0("<strong>Bureau: </strong>",bureaux_de_vote$name, 
                 "<br> <strong>Abstention: </strong>", bureaux_de_vote$abstention, "%")
  
  
  # == Création de la map ==
  
  output$mymap <- renderLeaflet({
    leaflet(bureaux_de_vote, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = 2.45, lat = 48.864, zoom = 14) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=13, maxZoom=18))  %>%
      addPolygons(fillColor = ~pal(abstention),  color = "black", fillOpacity = 0.9, popup = popup) %>%
      addLegend("bottomright", pal = pal, values = ~abstention,
                title = "Taux d'abstention par bureau de vote",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1, labels = "taux_abstention"
      )
  })
  
})

# == Application ==============================================

shinyApp(ui = ui, server = server)

