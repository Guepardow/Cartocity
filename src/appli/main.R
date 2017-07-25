# = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 1er juillet 2017
# CartoMontreuil - Dataviz d'une ville
#
# Auteur : Mehdi Miah
#
# Descriptif: Visualise l'ensemble des données de la ville de Montreuil
#
# Remarques : pour l'instant, seules les présidentielles 2017 sont disponibles
#
# TODO  : à chaque candidat sa couleur
#       : obtenir plus d'info pour bureau de vote
#
# BUG : encoding UTF-8 non active sur geojson_read
#     : bureaux_de_vote : code couleur de 17 sur la carte des bureaux 2015
#                       : nom 8 + 10 + 9 +
# = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == Préambule ======================================

rm(list = ls())
cat("\014")

setwd("C:/Users/Mehdi/Desktop/Cartocity")

library(dplyr)
library(data.table)
library(tidyr)
library(shiny)
library(leaflet)
library(geojsonio)

# Il faut calculer les pourcentages des voix emportés par le candidat par ce candidat
source("./src/appli/get_voix.R")
source("./src/appli/get_max_voix.R")

# == Ouverture des fichiers permettant la géolocalisation ===================

# Liste des élections disponibles : ensemble des dossiers présents dans ./data/vote
liste_elections = list.dirs(path = "./data/vote", full.names = FALSE, recursive = FALSE)

# Liste des couleurs des candidats
# TODO : réactive
liste_couleurs = fread("./data/vote/2017 - Presidentielles/color.csv", stringsAsFactors = FALSE, data.table = FALSE)

# carte des bureaux de vote
# TODO : a rendre reactive
bureaux_de_vote = geojson_read("./data/bureaux_de_vote/output/montreuil_57s.json", what = "sp")

# == User interface ============================================

ui = fluidPage(
  
  #Titre
  titlePanel("Etude cartographique de Montreuil"),
  
  tabsetPanel(
    
    #Panel 1 : la carte des élections
    tabPanel(title = "Carte des élections", 
             
             # 1ere colonne : le choix de l'élection et du tour
             column(width = 3,
                    
                    # Choix de l'élection
                    selectInput(inputId = "election_choice",
                                label = "Choix de l'élection",
                                choices = liste_elections
                               ),
                    
                    # Choix du tour
                    radioButtons(inputId = "tour_choice", 
                                 label = "Choix du tour", 
                                 inline = TRUE,
                                 choices = c("1er tour", "2nd tour") #TODO : s'adapter à l'élection
                                 )
                    
                    ),
             
             # 2e colonne : option de visualisation
             column(width = 3,
                    
                    # Choix de la méthode de calcul
                    radioButtons(inputId = "calcul_choice",
                                label = "Choix de la méthode de calcul",
                                choices = c("Brute", "Officielle"),
                                selected = "Officielle",
                                inline = TRUE
                    ),
                    
                    # Choix de la colorimétrie
                    radioButtons(inputId = "color_choice",
                                 label = "Choix de la colorimétrie",
                                 choices = c("Par rapport à l'ensemble", "Par rapport au candidat"),
                                 selected = "Par rapport au candidat"
                    )
                    
                    
             ),
             
             # 3e colonne : le choix du candidat (y compris abstention, nul et blanc)
             column(width = 3,
                    
                    # Choix du candidat
                    selectInput(inputId = "candidat_choice",
                                label = "Choix du vote",
                                choices = ""
                    )
                    
             ),
             
             leafletOutput("mymap", height = 650) %>% print,
             
             # Shiny versions prior to 0.11 should use class="modal" instead.
             absolutePanel(id = "panel_controls", class = "panel panel-default", fixed = TRUE,
                           draggable = FALSE, top = 90, left = "auto", right = 20, bottom = "auto",
                           width = 330, height = "auto",
                           
                           h3("Bureau de vote")
             )
    ),
    
    #Panel 2 : la carte des données socio-économique
    tabPanel(title = "Carte socio-économique"),
    
    #Panel 3 : l'analyse des comportements des bureaux
    tabPanel(title = "Analyse des bureaux"), 
    
    #Panel 4 : modèle de panel data pour déterminer la répartition des voix
    tabPanel(title = "Répartition des votes au second tour"),
    
    #Panel 5 : explication de la méthodologie
    tabPanel(title = "Méthodologie")
  )
)

# == Server ====================================================

server = shinyServer(function(input, output, session) {

  # == Ouverture du fichier des élections du tour correspond ==
  election_tour = reactive({
    fread(paste0("./data/vote/", input$election_choice, "/", input$tour_choice, "/output/montreuil.csv"), data.table = FALSE, encoding = "UTF-8")
  })
  
  # == Ajout du candidat sélectionné ==
  liste_candidats = reactive({
    election_tour() %>% dplyr::select(choix) %>% unique()
  })
  
  #Mise à jour de l'affichage des candidats
  observe({
    updateSelectInput(session, "candidat_choice", choices = liste_candidats()
    )
  })
  
  # == Calcul du score maximal ==
  max_voix = reactive({
    round(get_max_voix(election_tour(), input$calcul_choice),2) + 1 
  })
  
  # == Calcul des scores du candidat choisi ==
  newData <- reactive({
    df_voix <- get_voix(election_tour(), input$candidat_choice, calcul = input$calcul_choice)
    return(df_voix)
  })
  
  # == Colorimétrie ==
  
  newPal <- reactive({
    if(input$color_choice == "Par rapport à l'ensemble"){
      pal <- colorNumeric(palette = "Reds", domain = 0:max_voix())
    }else if(input$color_choice == "Par rapport au candidat"){
      pal <- colorNumeric(palette = "Reds", domain = bureaux_de_vote$pct)
    }
    return(pal)
  }) 
  
  # == Création de la map ==
  
  output$mymap <- renderLeaflet({
    
    df_voix = newData()
    bureaux_de_vote@data <- left_join(bureaux_de_vote@data, df_voix, by = "num_bureau")
    
    # Colorimétrie
    pal = newPal()
    
    # == Nom des bureaux de vote ==

    leaflet(bureaux_de_vote, 
            options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = 2.45, lat = 48.864, zoom = 14) %>%
      
      addProviderTiles("CartoDB.Positron", 
                       options = providerTileOptions(minZoom=13, maxZoom=18))  %>%
      addPolygons(fillColor = ~pal(pct),  
                  color = "black",
                  fillOpacity = 0.9, 
                  layerId = bureaux_de_vote$name,
                  popup = paste0("<strong>Bureau: </strong>",bureaux_de_vote$name, 
                                 "<br> <strong>Pourcentage: </strong>", bureaux_de_vote$pct, "%")) %>%
      addLegend("bottomright", 
                pal, 
                values = ~pct,
                title = "% de voix",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1, 
                labels = "taux_abstention"
      )
  })
  
  observe({
    
    #create object for clicked polygon
    click <- input$mymap_shape_click
    if (is.null(click))
      return()
    cat(sprintf(paste(click$id, "\n")))    
  })
  
})

# == Application ==============================================

shinyApp(ui = ui, server = server)
