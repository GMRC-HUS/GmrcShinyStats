#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody sidebarMenu menuItem tabItem tabItems
#' @noRd

#source("./theme.R", local = TRUE)



### creating custom theme object
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(205, 92, 92)"
  
  ,headerButtonBackColor = "rgb(205, 92, 92) "
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(205, 92, 92) "
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(64,64,64)"
    ,colorMiddle = "rgb(96,96,96)"
    ,colorEnd = "rgb(192,192,192)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(96,96,96)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(205, 148, 148,1)"
    ,colorMiddle = "rgba(255, 240, 240,1)"
    ,colorEnd = "rgba(255,240,240,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(205, 148, 148,1)"
    ,colorMiddle = "rgba(255, 240, 240,1)"
    ,colorEnd = "rgba(255,240,240,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(60,179,113,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    
    #addResourcePath("www", tempdir),
    
    # Your application UI logic
    navbarPage(
      "G.M.R.C.",
                tabPanel("Avec une base de données",
                         dashboardPage(
                            dashboardHeader(title = tagList(img(src = "www/logo1.png", height = "30px"), "Base de données")),
                                       dashboardSidebar(
                                         sidebarMenu(
                                          #shinythemes::themeSelector(),
                                            menuItem("Accueil", tabName = "accueil", icon = icon("fa-star", verify_fa = FALSE)),
                                            menuItem("Base de données", tabName = "base", icon = icon("fa-database", verify_fa = FALSE)),
                                            menuItem("Descriptif", tabName = "descriptif", icon = icon("fa-percent", verify_fa = FALSE)),
                                            menuItem("Croisements/Inférence", tabName = "croisements", icon = icon("fa-dice", verify_fa = FALSE)),
                                            menuItem("Analyse de survie", tabName = "survie", icon = icon("fa-skull-crossbones", verify_fa = FALSE)),
                                            menuItem("Tests diagnostiques", tabName = "tests", icon = icon("fa-vial", verify_fa = FALSE)),
                                            menuItem("Concordance", tabName = "concordance", icon = icon("fa-equals", verify_fa = FALSE)),
                                            menuItem("Historique", tabName = "historique", icon = icon("fa-clock-rotate-left", verify_fa = FALSE))
                                         )
                                       ),
                                       dashboardBody(
                                         
                                          # dashboardthemes::shinyDashboardThemes(
                                          #   theme = "custom_theme"
                                          # ),
                                         customTheme,
                                         tabItems(
                                           # First tab content
                                           tabItem(tabName = "accueil",
                                                   mod_Accueil_ui("Accueil_1")
                                           ),

                                           # Second tab content
                                           tabItem(tabName = "base",
                                                   mod_chargement_ui("chargement_1")
                                           ),

                                           #Third tab content
                                           tabItem(tabName = "descriptif",
                                                   mod_Descriptifs_ui("Descriptifs_1")
                                           ),
                                           tabItem(tabName = "croisements",
                                                   mod_Croisements_ui("Croisements_1")
                                           ),
                                           tabItem(tabName = "survie",
                                                   mod_Survie_ui("Survie_1")
                                           ),
                                           tabItem(tabName = "tests",
                                                   mod_Tests_ui("Tests_1")
                                           ),
                                            tabItem(tabName = "concordance",
                                                    mod_Concordance_ui("Concordance_1")
                                            ),
                                            tabItem(tabName = "historique",
                                                    mod_Historique_ui("Historique_1")
                                            )
                                         )
                                       )
                         )
                         ),

                tabPanel("Saisie manuelle",
                         mod_SaisieManuelle_ui("SaisieManuelle_1")),



                tabPanel("Rédaction",
                         mod_Redaction_ui("Redaction_1"))
                




    )
    )
}



# tagList(
#   # Leave this function for adding external resources
#   golem_add_external_resources(),
#   # Your application UI logic
#   tabsetPanel(id="Panneau",
#               tabPanel("Accueil",
#                        mod_chargement_ui("chargement_1")
#               ))
#
# )




#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    system.file('app/www', package = 'GmrcShinyStats')
  )
  

  tags$head(
    favicon(),
    tags$style(src = "www/custom.css"),
    # bundle_resources(
    #   path =  system.file('app/www', package = 'GmrcShinyStats'),
    #   app_title = "GmrcShinyStats"
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )


}
