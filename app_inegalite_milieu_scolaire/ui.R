# library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title=HTML("Les inégalités dans le milieu scolaire"),# Titre de l'application
                    disable= FALSE,
                    titleWidth = 380,
                    tags$li(class="dropdown",tags$a(href="https://twitter.com/education_gouv", icon("twitter"), "Twitter", style="color:white")),
                    tags$li(class="dropdown",tags$a(href="https://www.education.gouv.fr/",  "Site Officiel", style="color:white")),
                    tags$li(class="dropdown",tags$a(href="https://www.instagram.com/education_gouv/?hl=fr",icon("instagram") , "Instagram", style="color:white")),
                    dropdownMenu(type="message", messageItem(from="Notification",message="Bienvenue sur notre application WEB!",icon=icon("envelope-open")))
    ), 
    dashboardSidebar( # Menu de l'application
      width = 220,
      sidebarMenu(
        menuItem("Accueil", tabName = "Accueil", icon = icon("home")),
        menuItem(HTML("Inégalités \n socio-économiques"), tabName = "social", icon = icon("money-bills")),
        menuItem("Inégalités territoriales",tabName = "geo", icon = icon("flag")),
        menuItem("Inégalités de genre", tabName = "genre",icon=icon("venus-mars")),
        menuItem("Bases de données",tabName = "BDD",icon = icon("database"))
      )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Accueil",
                tabsetPanel(
                  tabPanel("Informations générales",
                           fluidRow(style = 'margin:6px;'),
                           fluidRow(
                             box(title = "L'éducation nationale", status = "primary", solidHeader = TRUE,
                                 p("L'Éducation nationale en France est le ministère chargé de l'organisation et de la gestion du système éducatif national. 
                                        Il est responsable de la politique éducative, de la mise en œuvre des programmes scolaires, de la formation des enseignants et de la gestion des établissements scolaires publics.
                                        Le ministère de l'Éducation nationale est dirigé par un ministre nommé par le Président de la République, sur proposition du Premier ministre. 
                                        Le ministre est assisté d'un secrétaire d'État chargé de l'Enseignement supérieur et de la Recherche.
                                        L'Éducation nationale en France est chargée de garantir l'égalité des chances pour tous les élèves et de promouvoir la réussite scolaire."
                                 ))
                             )))),
                
                tabItem("social"),
                tabItem("geo"),
                tabItem("genre"),
                tabItem("BDD"))
      )
  ) # ferme le dashboarpage
) #ferme le ui