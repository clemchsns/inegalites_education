source("global.R",local=T)


# library(shiny)
library(shinydashboard)
library(plotly)


shinyServer(function(input, output) {
  ### Accueil
  
  # Value-Box
  output$educ_nationale <- renderValueBox({
    education_nationale
  })
  
  output$loi_Falloux <- renderValueBox({
    Loi_Falloux_Box
  })
  
  output$progres_filles <- renderValueBox({
    Progres_filles
  })
  
  output$gratuite <- renderValueBox({
    Gratuite
  })
  
  output$sep_eglise_etat <- renderValueBox({
    Separation_eglise_etat
  })
  
  output$filles_bac <- renderValueBox({
    Bac_filles
  })
  
  output$prog_identique_F_G <- renderValueBox({
    Prog_identiques
  })
  
  output$ecole_obl_16 <- renderValueBox({
    Ecole_obligatoire_16
  })
  
  output$mixite <- renderValueBox({
    Mixite_filles_garcons
  })
  
  output$creation_bac_pro <- renderValueBox({
    Creations_bacs_pros
  })
  
  ## Image personnages important
  
  output$Jules_FERRY <- renderImage({
    list(src="www/jules_FERRY.jpg",alt="Jules_FERRY",width=200,height=250,style='position : relative')
  },deleteFile=FALSE)
  
  output$Martin_LUTHER <- renderImage({
    list(src="www/Martin_LUTHER.jpg",alt="Martin_LUTHER",width=200,height=250,style='position : relative')
  },deleteFile=FALSE)
  
  output$Mustafa_Kemal_Atatürk <- renderImage({
    list(src="www/Mustafa_Kemal_Atatürk.jpg",alt="Mustafa_Kemal_Atatürk",width=200,height=250,style='position : relative')
  },deleteFile=FALSE)
  
  output$Francisco_Giner_de_los_Ríos <- renderImage({
    list(src="www/Francisco_Giner_de_los_Ríos.jpg",alt="Francisco_Giner_de_los_Ríos",width=200,height=250,style='position : relative')
  },deleteFile=FALSE)
  
  output$Mori_Arinori <- renderImage({
    list(src="www/Mori_Arinori.jpg",alt="Mori_Arinori",width=200,height=250,style='position : relative')
  },deleteFile=FALSE)
  
  ### Inégalités socio-économiques
  
  # Values-box
  output$bac_origine_sociale <- renderValueBox({
    valueBox(
      paste(baccalaureat,"%"),
      subtitle = " d'admis au baccalauréat quelque soit la classe sociale",
      icon = icon('graduation-cap'),
      color = "green"
    )
  })
  
  output$bac_sans_emploi <- renderValueBox({
    valueBox(
      paste(round(baccalaureat_sans_emploi$Pct_admis_baccalaureat),"%"),
      subtitle = "d'admis au baccalauréat avec des parents sans activite professionnelle",
      icon = icon('graduation-cap'),
      color = "red"
    )
  })
  
  output$bac_cadre <- renderValueBox({
    valueBox(
      paste(round(baccalaureat_cadre$Pct_admis_baccalaureat),"%"),
      subtitle = "d'admis au baccalauréat avec des parents cadres ou en professions intellectuelles supérieures",
      icon = icon('graduation-cap'),
      color = "green"
    )
  })
  
  # Classes sociales au college
  output$treemap_college <- renderPlot({
    
    df <- data.frame(
      group = c("très favorisé","favorisé","moyenne","défavorisé"),
      value = c(mean(fr_indicateur_segreg_college$proportion_tfav[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]),
                mean(fr_indicateur_segreg_college$proportion_fav[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]),
                mean(fr_indicateur_segreg_college$proportion_moy[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]),
                mean(fr_indicateur_segreg_college$proportion_defav[fr_indicateur_segreg_college$nom_dep==input$nom_departement&fr_indicateur_segreg_college$annee==input$annee]))
    )
    p <- treemap(df,
                 index="group",
                 vSize="value",
                 type="index",
                 border.col = "black",
                 palette = "Blues",
                 title="Répartition des classes sociales au collège")
    p
    
  })
  
  # PCS au lycee
  output$camembert_lycee <- renderPlotly({
    df_PCS_renomme <- data.frame("Categorie"= df_PCS$Origine_sociale, "Pct_admis_baccalaureat" = df_PCS$Pct_admis_baccalaureat)
    
    fig <- plot_ly(df_PCS_renomme, labels = ~Categorie, values = ~Pct_admis_baccalaureat, type = 'pie',
                   showlegend = FALSE)
  
    fig1 <- fig |> layout(title = 'Répartition des PCS au lycée',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    fig1
    
    
  })
  
  # Reussite bac selon PCS au lycee
  output$reussite_bac_PCS <- renderPlot({
    df_courbe_reussite_bac <- fr_reussite_bac |> dplyr::filter(Origine_sociale==input$origine_sociale)
    ggplot(df_courbe_reussite_bac)+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat general`,color="baccalauréat général",linewidth = 4))+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat technologique`,color="baccalauréat technologique",linewidth = 4))+
      geom_line(aes(x = Annee,y=`Pourcentage d'admis au baccalaureat professionnel`,col="baccalauréat professionnel",linewidth = 4))+
      labs(xlab = "Année", ylab = "Pourcentage d'admis",color="Bacs:",title="Réussite par baccalauréat selon la PCS")+
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Table reussite DNB selon le secteur privé / public 
  output$reussite_secteur <- renderDataTable({
    taux_reussite_secteur
  })
  
  
  # Classes sociales au college prive / public
  output$amchartComparaisonPCS <- renderAmCharts({
    comp_college_PU_PR
  })
  
  
  ### --- Inegalités territoriales ----
  
  # Evolution du nombre d'enseignant par élèves 
  # Premier pays
  output$evol_enseignant_eleves <- renderPlot({
    d <- enseignant_par_eleves |>
      dplyr::filter(LOCATION==input$Pays_enseignant) |> 
      group_by(TIME) |>
      mutate(nb_moy = mean(VALUE))
    
    m <- ggplot(d)+
      aes(x=TIME,y=nb_moy)+
      geom_line(color="#87cefa",linewidth = 4)+
      labs(xlabs = "Année", ylab="Nombre moyen d'enseignant par élèves", title = paste("Evolution du nombre d'enseignant par élèves en ",input$Pays_enseignant))+
      theme(plot.title = element_text(hjust = 0.5))
    m
    
  })
  
  # Deuxième pays
  output$evol_enseignant_eleves_2 <- renderPlot({
    e <- enseignant_par_eleves |>
      dplyr::filter(LOCATION==input$Pays_enseignant2) |> 
      group_by(TIME) |>
      mutate(nb_moy = mean(VALUE))
    
    m <- ggplot(e)+
      aes(x=TIME,y=nb_moy)+
      geom_line(linewidth = 4)+
      labs(xlabs = "Année", ylab="Nombre moyen d'enseignant par élèves", title = paste("Evolution du nombre d'enseignant par élèves en",input$Pays_enseignant2))+
      theme(plot.title = element_text(hjust = 0.5))
    m
    
  })
  
  # Carte du nombre d'enseignant par élèves
  # output$carte_evol_enseignant <- renderLeaflet({
  #   
  #   # INTRODUIRE UN REACTIVE
  #   enseignant_par_eleves <- enseignant_par_eleves |> 
  #     dplyr::filter(LOCATION==input$Pays_mobilite) |> 
  #     group_by(TIME) |> 
  #     mutate(nb_moy = mean(VALUE))
  #   
  #   world2 <- merge(x=world,y=enseignant_par_eleves,by.x="sov_a3",by.y="ACRONYME_PAYS")
  #   
  #   world3 <- world2 |> 
  #     dplyr::filter(TIME==input$annee_carte)
  #   
  #   coords <- st_coordinates(world3)
  #   longitude <- coords[,"X"]
  #   latitude <- coords[,"Y"]
  # 
  #   pal <- colorNumeric(scales::seq_gradient_pal(low = "yellow", high = "red",
  #                                                space = "Lab"), domain = world3$nb_moy)
  #   
  #   
  #   map <- leaflet() |>
  #     addTiles() |>
  #     setView(lng=0,lat=30,zoom=2) |>
  #     addPolygons(data = world3,color=~pal(nb_moy),
  #                 fillOpacity = 0.6) |>
  #                 # stroke = TRUE,weight=1,
  #                 # popup=~paste(as.character(NOM_DEPT),
  #                 #              as.character(t_prev),
  #                 #              sep=" : "),
  #                 # highlightOptions = highlightOptions(color = "black",
  #                 #                                     weight = 3,
  #                 #                                     bringToFront = TRUE)) %>%
  #     addLayersControl(options=layersControlOptions(collapsed = FALSE))
  # 
  #   map
  #   
  #                         
  #                        
  # })
  
  # Carte taux de réussite DNB par département
  output$carte_reussite_DNB <- renderPlot({
    dpt4 <- dpt3 |>
      dplyr::filter(Session==input$annee_geo) |>
      group_by(Session,geometry) |>
      as_tibble() |>
      st_as_sf()
    dpt4
    
    
    carte <- ggplot(dpt4)+
      aes(fill=reussite)+
      labs(title="Taux de réussite au DNB par département")+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_sf()
    carte
  })
  
  # Carte PCS majoritaire par département
  output$map_pcs_dpt <- renderLeaflet({
    carte_pcs
  })
  
  # Carte taux de scolarisation
  output$taux_scolarisation_FR <- renderPlot({
    carte_tx_scolarisation
  })
  
  # Etudiants en mobilite internationale
  output$mobilite <- renderAmCharts({
    etudiant_mobilite <- etud_mobilite |> 
      dplyr::filter(LOCATION==input$Pays_mobilite)
    
    amPlot(VALUE~TIME,data =etudiant_mobilite, type="l",main = "Evolution du nombre d'étudiants en mobilité internationale")
  })
  
  ### Inegalités de genre
  
  # Repartition des bacs 
  output$repartition_bac <- renderPlot ({voies
  })
  
  
  ### Sources
  
  # Affichage des bases de données
  selected_df <- reactive({
    DT::datatable(data=head(liste_df[[input$affichage_table]]))
  })
  
  output$table <- renderDataTable({
    selected_df()
  })
  
})