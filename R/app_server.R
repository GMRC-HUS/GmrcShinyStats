#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import dashboardthemes
#' @import utils
app_server <- function(input, output, session) {
  # Your application server logic
  r<-reactiveValues(
  )
  pasDeBase <-   fluidPage(   
    h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
    p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
  )
  

  
  
  mod_chargement_server("chargement_1",r)
  mod_Croisements_server("Croisements_1",r)
  mod_Survie_server("Survie_1",r)
  mod_Tests_server("Tests_1",r)
  mod_Concordance_server("Concordance_1",r)
  mod_Accueil_server("Accueil_1")
  mod_Descriptifs_server("Descriptifs_1",r)
  
  mod_SaisieManuelle_server("SaisieManuelle_1")
  mod_Redaction_server("Redaction_1")
  
  #callModule(mod_Descriptifs_server,id = "select",session = session, r = r)
}
