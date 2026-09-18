#' Redaction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Redaction_ui <- function(id){
  dashboardPage(dashboardHeader(title = "Rédaction"),
                dashboardSidebar(conditionalPanel(
                  condition = "!input.sidebarCollapsed",
                  tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
                                 tags$br(),tags$br(),
                                 tags$h4("En cas de question, vous pouvez contacter :",style = "color:white",align = "center"),
                                 tags$p(tags$b("GMRC CHU Strasbourg"),style = "color:white",align = "center"),
                                 tags$p(tags$b("ShinyGMRC@chru-strasbourg.fr"),style = "color:white",align = "center"),
                                 # tags$p("03 88 1(1 67 49)",style = "color:#08088A"),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
                                 #height = 400	, width = 492
                                 tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(), tags$br(),tags$br(),tags$br(),tags$br())),
                dashboardBody(
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        tags$br(),                              
                        img(src="www/logo1.png", height = "100%"	, width = "100%", style="display: block; margin-left: auto; margin-right: auto;")
                      ),
                    mainPanel(
                    tags$h3("Rédiger la partie matériel et méthodes",align = "center",style = "color:#08088A; font-family: Georgia; font-size : 30px;"),
                    tags$br(),
                    tags$p("On présente ci-dessous un exemple de rédaction de la partie « Matériel et Méthodes ». Cet exemple doit être adapté et modifié en fonction du type d'étude réalisée et du contexte. Le premier paragraphe correspond à un exemple type pour une utilisation des outils descriptifs, des croisements de variables et des tests d'associations univariés. Les parties suivantes sont spécifiques aux analyses de performances diagnostiques et aux mesures de concordance. Elles pourront être ajoutées à l'endroit symbolisé par deux astérisques/étoiles **.", style="text-align: justify"),
                    tags$br(),tags$br(),
                    tags$h4("Matériel et méthodes",align = "left",style = "color:#08088A"),
                    tags$p(tags$i("Les variables quantitatives ont été décrites à l'aide des statistiques usuelles de position et de dispersion, à savoir la moyenne, la médiane, la variance, le minimum, le maximum et les quantiles. Les variables qualitatives ont quant à elles été décrites avec les effectifs et les proportions de chaque modalité. Des proportions cumulées ont également été calculées pour les variables à plus de deux modalités."),style = "color:#08088A; text-align: justify"),
                    tags$p(tags$i("Le caractère Gaussien des variables quantitatives a été évalué à l'aide du test de Shapiro-Wilk. Si les conditions d'application étaient respectées, le lien entre deux variables quantitatives a été évalué à l'aide du test de corrélation linéaire de Pearson. Dans le cas contraire, un test de corrélation de Spearman a été réalisé. Pour la comparaison d'une variable quantitative entre plusieurs sous-groupes, une analyse de la variance ou le test de Kruskal et Wallis ont été utilisés, toujours en fonction des hypothèses d'utilisation de chacun de ces tests. Enfin pour le croisement entre plusieurs variables qualitatives, le test paramétrique du Chi2 a été utilisé si les conditions d'application le permettaient. Si ce n'était pas le cas, le test exact de Fisher a été réalisé."),style = "color:#08088A; text-align: justify"), tags$p(tags$i("**"),style = "color:#08088A; text-align: justify"),
                    tags$p(tags$i("Le risque de première espèce alpha a été fixé à 5% pour toutes les analyses. L'ensemble des analyses a été réalisé sur le logiciel R dans sa version 3.1, R Development Core Team (2008). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. via l’application GMRC Shiny Stat du CHU de Strasbourg (2017)."),style = "color:#08088A; text-align: justify"),
                    tags$br(),tags$br(),tags$br(),tags$br(),
                    tags$h5("** Performances diagnostiques",align = "left",style = "color:#08088A"),
                    tags$p(tags$i("Pour évaluer les performances diagnostiques d'un critère binaire sur une variable dépendante, les mesures de sensibilité, de spécificité, la valeur prédictive positive et la valeur prédictive négative ont été estimées à partir du tableau de contingence associé."), style="text-align: justify"),
                    tags$h5("** ROC",align = "left",style = "color:#08088A"),
                    tags$p(tags$i("Pour évaluer les performances diagnostiques d'une variable quantitative, une recherche du seuil optimal a été réalisée en utilisant une courbe ROC. Les sensibilités et spécificités ont été évaluées pour différents seuils et le seuil optimal a été défini par maximisation de l'indice de Youden. L'intervalle de confiance de la courbe ROC ainsi que le test de la différence de l'aire sous la courbe à 0.50 a été estimé par simulations bootstrap sur les mesures de performance du critère. Pour le seuil alors défini, les critères de performance habituels ont été évalués à savoir : la sensibilité, la spécificité, les valeurs prédictives positive et négative."), style="text-align: justify"),
                    tags$h5("** Concordance",align = "left",style = "color:#08088A"),
                    tags$p(tags$i("La concordance entre deux lectures qualitatives a été évaluée par le coefficient Kappa de Cohen. Le coefficient est présenté sur une échelle [-1;1] allant de façon continue de la discordance à la concordance totale. L'intervalle de confiance associé est évalué par 1000 réplications bootstrap, et la méthode choisie est l'estimation de l'intervalle de type bca (bias corrected and accelerated, Efron 1987)."), style="text-align: justify"),
                    tags$h5("** Survie",align = "left",style = "color:#08088A"),
                    tags$p(tags$i("Une analyse de survie a été réalisée en considérant le délai jusqu'à apparition de l'évènement. Les sujets n'ayant pas présenté l'évènement étaient censurés à la date des dernières nouvelles. Dès lors, une courbe de Kaplan-Meier a permis d'étudier l'évolution du taux de survie en fonction du temps dans son intervalle de confiance associé. Une comparaison entre plusieurs groupes de l'évolution de la survie a été réalisée en utilisant le test du Log-Rank associé aux courbes de Kaplan-Meier."), style="text-align: justify")
                    ))
                  )
                ))
}
    
#' Redaction Server Functions
#'
#' @noRd 
mod_Redaction_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Redaction_ui("Redaction_1")
    
## To be copied in the server
# mod_Redaction_server("Redaction_1")
