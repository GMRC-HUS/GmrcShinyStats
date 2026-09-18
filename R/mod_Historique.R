#' Historique UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Historique_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Historique des résultats"),
    p("Cette page liste les analyses réalisées au cours de la session en cours. L'historique est
      conservé uniquement en mémoire de l'application : aucune donnée ni aucun résultat n'est
      enregistré sur un serveur. L'historique est supprimé à la fermeture de l'application."),
    fluidRow(
      splitLayout(cellWidths = c("30%","70%"),
                  downloadButton(ns('DLhistorique'), label = "Télécharger l'historique (CSV)", class = "butt"),
                  actionButton(ns('Viderhistorique'), "Vider l'historique", class = "butt")
      )
    ),
    br(),
    h4("Analyses de la session"),
    tableOutput(ns('tableHistorique'))
  )
}

#' Historique Server Functions
#'
#' @noRd
mod_Historique_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tableHistorique <- renderTable({
      if (is.null(r$historique) || nrow(r$historique) == 0) {
        data.frame(message = "Aucune analyse réalisée pour le moment.")
      } else {
        r$historique
      }
    }, rownames = FALSE)

    output$DLhistorique <- downloadHandler(
      filename = function() paste0("historique_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
      content = function(file) {
        if (is.null(r$historique) || nrow(r$historique) == 0) {
          to_write <- data.frame(message = "Aucune analyse realisee")
        } else {
          to_write <- r$historique
        }
        write.csv(to_write, file, row.names = FALSE)
      }
    )

    observeEvent(input$Viderhistorique, {
      r$historique <- NULL
    })
  })
}

## To be copied in the UI
# mod_Historique_ui("Historique_1")

## To be copied in the server
# mod_Historique_server("Historique_1", r)
