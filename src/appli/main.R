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
# TODO  : couleur des polygones en fonction des candidats
#       : regrouper des candidats dans la version de calcul 'Brute'
#       : afficher le rang dans le bureau de vote du candidat sélectionné
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
library(ggplot2)

# Il faut calculer les pourcentages des voix emportés par ce candidat
source("./src/appli/get_voix_candidat.R")
source("./src/appli/get_max_voix.R")
source("./src/appli/get_voix_bureau.R")


# == Ouverture des fichiers permettant la géolocalisation ===================

# Liste des élections disponibles : ensemble des dossiers présents dans ./data/vote
liste_elections = list.dirs(path = "./data/vote", full.names = FALSE, recursive = FALSE)

# carte des bureaux de vote TODO : a rendre reactive
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
                    selectInput(inputId = "choice_election",
                                label   = "Choix de l'élection",
                                choices = liste_elections
                               ),
                    
                    # Choix du tour
                    radioButtons(inputId = "choice_tour", 
                                 label   = "Choix du tour", 
                                 inline  = TRUE,
                                 choices = c("1er tour", "2nd tour") #TODO : s'adapter à l'élection
                                 )
                    
                    ),
             
             # 2e colonne : option de visualisation
             column(width = 3,
                    
                    # Choix de la méthode de calcul
                    radioButtons(inputId = "choice_version_calcul",
                                label    = "Choix de la méthode de calcul",
                                choices  = c("Brute", "Officielle"),
                                selected = "Officielle",
                                inline   = TRUE
                    ),
                    
                    # Choix de la colorimétrie
                    radioButtons(inputId = "choice_color",
                                 label = "Choix de la colorimétrie",
                                 choices = c("Par rapport à l'ensemble", "Par rapport au candidat"),
                                 selected = "Par rapport au candidat"
                    )
                    
                    
             ),
             
             # 3e colonne : le choix du candidat (y compris abstention, nul et blanc)
             column(width = 2,
                    
                    # Choix du candidat
                    selectInput(inputId = "choice_candidat",
                                label   = "Choix du vote",
                                choices = ""
                    )
                    
             ),
             
             # Carte interactive
             leafletOutput("mymap", height = 650) %>% print,
             
             # Shiny versions prior to 0.11 should use class="modal" instead.
             absolutePanel(id        = "panel_controls", 
                           class     = "panel panel-default", 
                           fixed     = FALSE,
                           draggable = TRUE, 
                           top       = 260, 
                           left      = "auto", 
                           right     = 20, 
                           bottom    = "auto",
                           width     = 420, 
                           height    = "auto",
                           
                           h3("Bureau de vote",  align = "center"), 
                           
                           # Texte interactif
                           textOutput("text_bureau"),
                           
                           # Barplot interactif
                           plotOutput("my_barplot")
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
    fread(paste0("./data/vote/", input$choice_election, "/", input$choice_tour, "/montreuil.csv"), 
          data.table = FALSE, encoding = "UTF-8")
  })
  
  # == Ouverture du fichier des couleurs politiques ==
  liste_couleurs = reactive({
    fread(paste0("./data/vote/", input$choice_election, "/color.csv"), stringsAsFactors = FALSE, data.table = FALSE)
  })
  
  # == Ajout du candidat sélectionné ==
  liste_candidats = reactive({
    election_tour() %>% dplyr::select(choix) %>% unique()
  })
  
  #Mise à jour de l'affichage des candidats
  observe({
    updateSelectInput(session, "choice_candidat", choices = liste_candidats()
    )
  })
  
  # == Calcul du score maximal ==
  # Cela permet d'obtenir le score maximal lorsque l'utilisateur souhaite comparer selon la colorimétrie les candidats
  max_voix = reactive({
    round(get_max_voix(election_tour(), input$choice_version_calcul)) + 1 
  })
  
  # == Calcul des scores du candidat choisi ==
  get_df_voix_candidat <- reactive({
    df_voix_candidat <- get_voix_candidat(election_tour(), input$choice_candidat, version_calcul = input$choice_version_calcul)
    return(df_voix_candidat)
  })
  
  # == Colorimétrie ==
  
  get_pal = reactive({
    if(input$choice_color == "Par rapport à l'ensemble"){
      pal = colorNumeric(palette = "Reds", domain = 0:max_voix())
    }else if(input$choice_color == "Par rapport au candidat"){
      pal = colorNumeric(palette = "Reds", domain = bureaux_de_vote$pct)
    }
    return(pal)
  }) 
  
  # == Création de la map ==
  
  output$mymap = renderLeaflet({
    
    df_voix_candidat = get_df_voix_candidat()
    bureaux_de_vote@data = left_join(bureaux_de_vote@data, df_voix_candidat, by = "num_bureau")
    
    # Colorimétrie
    pal = get_pal()
    
    # Label
    labels <- sprintf(
      "<strong> Bureau : </strong> %s <br/>
      <strong>%s : </strong> %s&#37; <br/>
      <strong> Voix : </strong> %s / %s", # le symbole "%" est "&#37;" en HTML
      bureaux_de_vote$name, input$choice_candidat, bureaux_de_vote$pct, bureaux_de_vote$vote, bureaux_de_vote$count
    ) %>% lapply(htmltools::HTML)
    
    # == Nom des bureaux de vote ==

    leaflet(bureaux_de_vote, 
            options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = 2.47, lat = 48.864, zoom = 14) %>%
      
      addProviderTiles("CartoDB.Positron", 
                       options = providerTileOptions(minZoom = 13, maxZoom = 18))  %>%
      addPolygons(fillColor   = ~pal(pct),  
                  color       = "black",
                  fillOpacity = 0.9, 
                  layerId     = bureaux_de_vote$num_bureau,
                  label = labels,
                  highlightOptions = highlightOptions(weight = 5,color = "#666", bringToFront = TRUE),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px", clickable = TRUE,
                    direction = "auto")
                  ) %>%
      addLegend(position  = "bottomleft", 
                pal       = pal, 
                values    = ~pct,
                title     = "% de voix",
                labFormat = labelFormat(suffix = "%"),
                opacity   = 1.0, 
                labels    = "taux_abstention"
      )
  })
  
  # Création de l'objet click
  click = reactive({
    if (is.null(input$mymap_shape_click))
      return()
    return(input$mymap_shape_click)
  })
  
  # == Calcul des scores dans le bureau choisi ==
  get_df_voix_bureau <- reactive({
    df_voix_bureau <- get_voix_bureau(election_tour(), click()$id, version_calcul = input$choice_version_calcul, liste_couleurs())
    return(df_voix_bureau)
  })
  
  # == Génération du texte sur le bureau de vote ==
  output$text_bureau = renderText({
    click()$id
  })
  
  # == Génération de la répartition des voix ==
  output$my_barplot = renderPlot({
    if (is.null(input$mymap_shape_click))
      return()
    
    df_voix_bureau = get_df_voix_bureau()
    df_voix_bureau$choix <- factor(df_voix_bureau$choix, levels = df_voix_bureau$choix[order(df_voix_bureau$pct)]) #pour ordonner

    par(mar=c(3,8,0,3)) #haut, droite, bas, gauche
    ggplot(df_voix_bureau, aes(x=choix, y=pct, fill = couleur)) + 
      geom_bar(stat="identity", color="black") +  
      scale_fill_identity() + #pour obtenir la bonne couleur pour chaque candidat
      #ylim(c(0,max_voix())) + #valeur maximale sur l'axe des pct
      xlab("") + ylab("") + #no labels
      coord_flip() #format horizontal
  })
  
})

# == Application ==============================================

shinyApp(ui = ui, server = server)
