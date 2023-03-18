source("global.R",local=T)


# library(shiny)
library(shinydashboard)


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
  

  
})