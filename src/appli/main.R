# = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Création le 1er juillet 2017
# Dataviz d'une ville
#
# Mehdi Miah
#
# TODO  : rendrele 1er tour / 2nd tour reactive à l'élection
#       : https://stackoverflow.com/questions/40153870/r-shiny-how-to-filter-a-dataframe-before-outputting-a-merged-spatialpolygonsdat
#
# BUG : encoding UTF-8 non active sur geojson_read
#     : bureaux_de_vote : code couleur de 17 sur la carte des bureaux 2015
#                       : nom 8 + 10 + 9 +
#     : voix exprimés ou voix votés
# = = = = = = = = = = = = = = = = = = = = = = = = = = 

# == Préambule ======================================

rm(list = ls())
cat("\014")

setwd("C:/Users/Mehdi/Desktop/Cartocity")

library(dplyr)
library(data.table)
library(shiny)
library(leaflet)
library(geojsonio)


# Il faut calculer les pourcentages des voix emportés par le candidat par ce candidat
get_voix = function(election_tour, mon_choix){
  
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



# == Ouverture des fichiers permettant la géolocalisation ===================

# Liste des élections disponibles : ensemble des dossiers présents dans ./data/vote
liste_elections = list.dirs(path = "./data/vote", full.names = FALSE, recursive = FALSE)

# carte des bureaux de vote
# TODO : a rendre reactive
bureaux_de_vote = geojson_read("./data/bureaux_de_vote/output/montreuil_57s.json", what = "sp")

# debug =======================
election_tour = fread("./data/vote/2017 - Presidentielles/1er tour/output/montreuil.csv", data.table = FALSE, encoding = "UTF-8")
liste_candidats = election_tour$choix %>% unique()

#voix = get_voix(election_tour, "abstention")
#merger = merge(election_tour, voix, by = c("num_bureau", "choix"))
# ==================

#df_voix = get_voix(election_tour, "abstention")
#bureaux_de_vote@data <- left_join(bureaux_de_vote@data, df_voix)
#pal <- colorNumeric(palette = "Reds", domain = bureaux_de_vote$pct)
#popup <- paste0("<strong>Bureau: </strong>",bureaux_de_vote$name, 
#                "<br> <strong>Pourcentage: </strong>", bureaux_de_vote$pct, "%")

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
             
             # 2e colonne : le choix du candidat (y compris abstention, nul et blanc)
             column(width = 3,
                    
                    # Choix du candidat
                    selectInput(inputId = "candidat_choice",
                                label = "Choix du vote",
                                choices = liste_candidats #"abstention" #liste_candidats
                    )
                    
             ),
             
             leafletOutput("mymap", height = 550) %>% print
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
  
 # election_tour = reactive({
#    fread(paste0("./data/vote/", input$election_choice, "/", input$tour_choice, "/output/montreuil.csv"), data.table = FALSE, encoding = "UTF-8")
#  })
  
  
  # == Ajout du candidat sélectionné ==

 # liste_candidats = reactive({
#    election_tour() %>% select(choix) %>% unique()
#  })
  
  
  #Mise à jour de l'affichage des candidats
  #observe({
  #  updateSelectInput(session, "candidat_choice", choices = liste_candidats()
  #  )
  #})
  
  # == Reactive dataset ==
  newData <- reactive({
    isolate({
      df_voix <- get_voix(election_tour, input$candidat_choice)
    })
    return(df_voix)
  })
  
  df_voix = reactive({
    get_voix(election_tour, input$candidat_choice)
  })

 
  # == Création de la map ==
  
  output$mymap <- renderLeaflet({
    
    df_voix = newData()
    bureaux_de_vote@data <- merge(bureaux_de_vote@data, df_voix())
    
    # == Colorimétrie ==
    
    pal <- colorNumeric(palette = "Reds", domain = bureaux_de_vote$pct)
    
    # == Nom des bureaux de vote ==
    
    popup <- paste0("<strong>Bureau: </strong>",bureaux_de_vote$name, 
                    "<br> <strong>Pourcentage: </strong>", bureaux_de_vote$pct, "%")
    
  
    leaflet(bureaux_de_vote, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = 2.45, lat = 48.864, zoom = 14) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=13, maxZoom=18))  %>%
      addPolygons(fillColor = ~pal(bureaux_de_vote$pct),  color = "black",
                  fillOpacity = 0.9, popup = popup) %>%
      addLegend("bottomright", pal = pal, values = ~bureaux_de_vote$pct,
                title = "Taux d'abstention par bureau de vote",
                labFormat = labelFormat(suffix = "%"),
                opacity = 1, labels = "taux_abstention"
      )
  })
  
})

# == Application ==============================================

shinyApp(ui = ui, server = server)

