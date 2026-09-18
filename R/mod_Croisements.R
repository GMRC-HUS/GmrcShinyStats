#' Croisements UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import xtable
#' @import dplyr

mod_Croisements_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  uiOutput(ns('CroisementsInference')) 
}
    
#' Croisements Server Functions
#'
#' @noRd 
mod_Croisements_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    
    pasDeBase <-   fluidPage(   
      h4("Aucune base n'a été chargée en mémoire, cet onglet n'est pas accessible." ),
      p("Pour charger une base de données, rendez-vous sur l'onglet « Base de Données » dans la barre latérale.")
    )
    
    CroisementsInference<-
      fluidPage(navbarPage(id="Panel 2.x",title = NULL,
                           
                           tabPanel("Croisement 2 à 2",
                                    
                                    
                                    fluidPage( #includeCSS("./www/tables.css"),
                                      tags$head(
                                        tags$style(HTML("
.pure-table {
    /* Remove spacing between table cells (from Normalize.css) */
    border-collapse: collapse;
    border-spacing: 0;
    empty-cells: show;
    border: 1px solid #cbcbcb;
	text-align: center;
}

.pure-table caption {
    color: #000;
    font: italic 85%/1 arial, sans-serif;
    padding: 1em 0;
    text-align: center;
}

.pure-table td,
.pure-table th {
    border-left: 1px solid #cbcbcb;/*  inner column border */
    border-bottom: 1px solid #cbcbcb;

    font-size: inherit;
    margin: 0;
    overflow: visible; /*to make ths where the title is really long work*/
    padding: 0.5em 1em; /* cell padding */
	text-align: center;
}

.pure-table tr:hover {background-color: #f5f5f5}

/* Consider removing this next declaration block, as it causes problems when
there's a rowspan on the first cell. Case added to the tests. issue#432 */
.pure-table td:first-child,
.pure-table th:first-child {
    border-left-width: 0;
}

.pure-table thead {
    background-color: #e0e0e0;
    color: #000;
    text-align: left;
    vertical-align: bottom;
}

/*
striping:
   even - #fff (white)
   odd  - #f2f2f2 (light gray)
*/
.pure-table td {
    background-color: transparent;
}
"))
                                      ),
                                               titlePanel("Analyses descriptives croisées"),
                                               sidebarLayout( 
                                                 sidebarPanel(
                                                   
                                                   uiOutput(ns("propositionsCROISE1")),
                                                   radioButtons(ns('qualiquantiCROISE1'), 
                                                                "Nature de la variable",
                                                                c(Quantitative='quant', Qualitative='qual'),
                                                                'quant'),
                                                   uiOutput(ns("propositionsCROISE2")),
                                                   radioButtons(ns('qualiquantiCROISE2'), 
                                                                "Nature de la variable",
                                                                c(Quantitative='quant', Qualitative='qual'),
                                                                'quant'),
                                                   conditionalPanel(
                                                     condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",
                                                     radioButtons(ns('NATableau'), 
                                                                  "Afficher les données manquante",
                                                                  c(Non="no", Oui='always'),
                                                                  "no"))
                                                   
                                                   
                                                   
                                                 ),
                                                 mainPanel(  
                                                   fluidRow(
                                                     splitLayout(cellWidths = c("30%","70%"), 
                                                                 downloadButton(ns('PDFcroisements'),label="AIDE et Détails",class = "butt"),
                                                                 h4("Faites attention s'il y a un filtre")  
                                                     )
                                                   ),#finFluidRow
                                                   
                                                   tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                                   h3("Représentation graphique du lien entre les deux variables"),
                                                   plotOutput(ns('plotCROISE' )),
                                                   # debut conditionnal panel QualiQuali
                                                   conditionalPanel(
                                                     condition = "input.qualiquantiCROISE1 == 'qual' && input.qualiquantiCROISE2 == 'qual'",ns=ns,
                                                     h3("Tableau croisé",align = "left",style = "color:#08088A"),
                                                     tableOutput(ns("montableauCroisAUTO")),br(),
                                                     tableOutput(ns("montableauCroise2AUTO")),
                                                     tableOutput(ns("montableauCroise3AUTO")),
                                                     h3("Tests d'association / Comparaison des proportions",align = "left",style = "color:#08088A"),
                                                     fluidRow(
                                                       splitLayout(cellWidths = c("50%","50%"), 
                                                                   tableOutput(ns('AUTOtableCHI2')), 
                                                                   tableOutput(ns('AUTOtableFISHER'))
                                                       )
                                                     ),
                                                     textOutput(ns('AUTOCHI2conditions')),
                                                     h3("Rapport de cotes",align = "left",style = "color:#08088A"),
                                                     tableOutput(ns('oddratioAUTO'))
                                                   ),# fin panelQualiQuali,
                                                   # debut conditionnal panel QuantiQuali
                                                   conditionalPanel(
                                                     condition = "input.qualiquantiCROISE1 != input.qualiquantiCROISE2",ns=ns,
                                                     h3("Descriptif complet",align = "left",style = "color:#08088A"),
                                                     tableOutput(ns('descr3DESCRIPTIF')),
                                                     h3("Tests de comparaisons:",align = "left",style = "color:#08088A"),
                                                     verbatimTextOutput (ns("descr3TestNormalite")),
                                                     verbatimTextOutput (ns("descr3Testpv")),
                                                     verbatimTextOutput (ns("descr3TestsNPv")),
                                                     verbatimTextOutput (ns("descr3Tests_de_Student")),
                                                     verbatimTextOutput(ns("descr3TestsMANN")),
                                                     verbatimTextOutput (ns("ChoixSortieCROISE"))
                                                   ), # fin Panel Quali Quanti
                                                   # debut conditionnal panel QuantiQuanti
                                                   conditionalPanel(
                                                     condition = "input.qualiquantiCROISE1 == 'quant' && input.qualiquantiCROISE2 == 'quant'",ns=ns,
                                                     h3("Corrélation entre deux variables quantitatives",align = "left",style = "color:#08088A"),
                                                     verbatimTextOutput (ns("CorrelationCROISE"))
                                                   ),# fin panelQuantiQuali,
                                                   plotOutput(ns('plotCROISE2'))
                                                 )# fin MainPanel
                                                 
                                               )# fin sidebarlayout
                                    ))# fin fluidpage
                           ,
                           
                           tabPanel("Tableau croisement",
                                    
                                    
                                    
                                    fluidPage(    
                                      titlePanel("Analyses descriptives croisées"),
                                      sidebarLayout( 
                                        sidebarPanel(
                                          # 
                                          uiOutput(ns("propositionsTableauCROISE")),
                                          uiOutput(ns("selectionVariablesCroisees1")),
                                          uiOutput(ns("selectionVariablesCroisees3")),
                                          uiOutput(ns("selectionVariablesCroisees2")),
                                          radioButtons(ns("tableauCroiseSimpli"),"Tableau avec abréviation :"
                                                       , c( Oui = 1, Non = 0),0),
                                          sliderInput(ns("nbDec"), "Nombre de decimales : ", min =0,
                                                      max = 5, value= 3, step = 1),
                                          downloadButton(ns('downloadData'), 'Télécharger la table')
                                          
                                          
                                        ),
                                        mainPanel(    
                                          #   fluidRow(
                                          # 
                                          #                 downloadButton('PDFcroisements',label="AIDE et Détails",class = "butt")
                                          #  
                                          #      
                                          # ),#finFluidRow
                                          
                                          # tags$head(tags$style(".butt{background-color:#E9967A;} .butt{color: black;}")),
                                          h3("Tableau de comparaison de population"),
                                          conditionalPanel(condition = "!is.null(input$VariableCroisees)",ns=ns, tableOutput(ns('tableauCroisement')))
                                          
                                        )# fin MainPanel
                                        
                                      )# fin sidebarlayout
                                    )# fin fluidpage
                           ) # fin tabPanel tableau Croisement                  
      ) # fin tabset
      )
    
    #source("./CodeSansDependance.R", local = TRUE)
    #source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    #eval(parse("./miseEnForme.R", encoding="UTF-8"))
    
    output$PDFcroisements = downloadHandler(
      filename    = '3_Croisements.pdf',
      content     = function(file) file.copy(system.file("app/www/3_Croisements.pdf", package = 'GmrcShinyStats'), file, overwrite = TRUE),
      contentType = 'application/pdf'
    ) 
    
    observe({
    output$CroisementsInference = renderUI({
      if(!r$BASEchargee) do.call(tabPanel,pasDeBase)
      else do.call(tabPanel,CroisementsInference)
      
      
    })



    output$propositionsCROISE1 <- renderUI({
      selectInput(ns("variableCROISE1"), "Variable:",   choices=r$noms)
    })

    output$propositionsCROISE2 <- renderUI({

      selectInput(ns("variableCROISE2"), "Variable:",   choices=r$noms)
    })



    output$plotCROISE <- renderPlot({
      base    <-r$BDD
      variableCROISE1 <-base[,input$variableCROISE1]
      variableCROISE2 <-base[,input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="quant"){print(ggpoints(variableCROISE1,variableCROISE2,nomx =input$variableCROISE1, nomy = input$variableCROISE2 ))}
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){boxplot(variableCROISE1~variableCROISE2, xlab=input$variableCROISE2, ylab=input$variableCROISE1)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){boxplot(variableCROISE2~variableCROISE1,xlab=input$variableCROISE1, ylab=input$variableCROISE2)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="qual"){

        # print(ggpie(as.factor(variableCROISE1),as.factor(variableCROISE2)))
        barplotCroise<-barplot_croise(base = base,var1=input$variableCROISE1,var2=input$variableCROISE2)
        print(barplotCroise)


      }
    })
    output$plotCROISE2 <- renderPlot({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="quant"){    print(correl(variableCROISE1,variableCROISE2, nomx=input$variableCROISE1 , nomy= input$variableCROISE2))}
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){     print(ggcompar(input$variableCROISE1,input$variableCROISE2,base))}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){     print(ggcompar(input$variableCROISE2,input$variableCROISE1,base))}
    })




    output$montableauCroisAUTO <- renderText({

      base    <-r$BDD
      x <-base[,input$variableCROISE1]
      y <-base[,input$variableCROISE2]
      Matrice    <-  addmargins(table(x,y, dnn = c(input$variableCROISE1,input$variableCROISE2), useNA =  input$NATableau))
      colnames(Matrice)<- ifelse(is.na(colnames(Matrice)), "Données Manquantes",colnames(Matrice))
      rownames(Matrice)<- ifelse(is.na(rownames(Matrice)), "Données Manquantes",rownames(Matrice))
      var2<-input$variableCROISE2
      var1<-input$variableCROISE1
      entete<-paste("<tr><th></th><th colspan='",dim(Matrice)[2],"'>",var2 ,"</td></tr>
                  <tr> <th>",var1 ," </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>",x, "</th>",sep = "") )),collapse=""),"</tr>",sep="")


      print.xtable(xtable( Matrice,caption = "Effectifs"),
                   size="footnotesize", #Change size; useful for bigger tables
                   include.rownames=T,
                   caption.placement="top",
                   hline.after=NULL,
                   include.colnames=FALSE,
                   add.to.row = list(pos = list(0),
                                     command=entete), type = "html",  html.table.attributes='class = "pure-table"  ',
                   print.results =F)
    })

    output$montableauCroise2AUTO <- renderText({
      base    <-r$BDD
      x <-base[,colnames(base)==input$variableCROISE1]
      y <-base[,colnames(base)==input$variableCROISE2]
      Matrice    <- addmargins(100 * prop.table(addmargins(table(x,y, useNA = input$NATableau), 1), 1), 2)
      colnames(Matrice)<- ifelse(is.na(colnames(Matrice)), "Données Manquantes",colnames(Matrice))
      rownames(Matrice)<- ifelse(is.na(rownames(Matrice)), "Données Manquantes",rownames(Matrice))
      var2<-input$variableCROISE2
      var1<-input$variableCROISE1
      entete<-paste("<tr><th></th><th colspan='",dim(Matrice)[2],"'>",var2 ,"</td></tr>
                  <tr> <th>",var1 ," </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>",x, "</th>",sep = "") )),collapse=""),"</tr>",sep="")


      print.xtable(xtable( Matrice,caption = "Pourcentages ligne", digits = 2),
                   size="footnotesize", #Change size; useful for bigger tables
                   include.rownames=T,
                   caption.placement="top",
                   hline.after=NULL,
                   include.colnames=FALSE,
                   add.to.row = list(pos = list(0),
                                     command=entete), type = "html",  html.table.attributes='class = "pure-table"  ',
                   print.results =F)


    })

    output$montableauCroise3AUTO <-renderText({
      base    <-r$BDD
      x <-base[,colnames(base)==input$variableCROISE1]
      y <-base[,colnames(base)==input$variableCROISE2]
      Matrice    <- addmargins(100 * prop.table(addmargins(table(x,y, useNA = input$NATableau), 2), 2), 1)
      colnames(Matrice)<- ifelse(is.na(colnames(Matrice)), "Données Manquantes",colnames(Matrice))
      rownames(Matrice)<- ifelse(is.na(rownames(Matrice)), "Données Manquantes",rownames(Matrice))
      var2<-input$variableCROISE2
      var1<-input$variableCROISE1
      entete<-paste("<tr><th></th><th colspan='",dim(Matrice)[2],"'>",var2 ,"</td></tr>
                  <tr> <th>",var1 ," </th>", paste(unlist(lapply(unlist(dimnames(as.matrix(Matrice))[2]), function(x) paste("<th>",x, "</th>",sep = "") )),collapse=""),"</tr>",sep="")


      print.xtable(xtable( Matrice,caption = "Pourcentages colonne", digits = 2),
                   size="footnotesize", #Change size; useful for bigger tables
                   include.rownames=T,
                   caption.placement="top",
                   hline.after=NULL,
                   include.colnames=FALSE,
                   add.to.row = list(pos = list(0),
                                     command=entete), type = "html",  html.table.attributes='class = "pure-table"  ',
                   print.results =F)





    })


    output$AUTOtableCHI2 <- shiny::renderTable({
      base    <-r$BDD
      x <-base[,colnames(base)==input$variableCROISE1]
      y <-base[,colnames(base)==input$variableCROISE2]
      Mat    <- table(x,y)
      CH2<-chisq.test(Mat,correct=FALSE)
      resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$p.value)
      colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
      rownames(resTESTS)<-"Résultat"
      resTESTS
    },rownames=TRUE,colnames=TRUE)

    output$AUTOtableFISHER <- shiny::renderTable({
      base    <-r$BDD
      x <-base[,colnames(base)==input$variableCROISE1]
      y <-base[,colnames(base)==input$variableCROISE2]
      Mat    <- table(x,y)
      FI2<-fisher.test(Mat)
      resTESTS<-t(t( FI2$p.value))
      colnames(resTESTS)<-c("Fisher pValue")
      rownames(resTESTS)<-"Résultat"
      resTESTS
    },rownames=TRUE)


    output$AUTOCHI2conditions <- renderText({
      base    <-r$BDD
      x <-base[,colnames(base)==input$variableCROISE1]
      y <-base[,colnames(base)==input$variableCROISE2]
      Mat    <- table(x,y)
      CH2<-chisq.test(Mat,correct=FALSE)
      FI2<-fisher.test(Mat)
      ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
             "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
    })

    output$oddratioAUTO <- renderTable({
      base    <-r$BDD
      x <-base[,colnames(base)==input$variableCROISE1]
      y <-base[,colnames(base)==input$variableCROISE2]
      Mat    <- table(x,y)

      Nblignes   <-dim(Mat)[1]
      Nbcolonnes <-dim(Mat)[2]
      if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
        FI2<-fisher.test(Mat)
        OR<-cbind(FI2$estimate , FI2$conf.int[[1]],FI2$conf.int[[2]])
        colnames(OR)<-c("Rapport de cotes","Borne inf 2.5","Borne Sup 97.5")
        rownames(OR)<-"Résultat"}
      OR
    }, caption = "Rapport de cotes et IC",
    caption.placement = getOption("xtable.caption.placement", "bottom"),
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)


    output$descr3DESCRIPTIF<- renderTable({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2,nom = input$variableCROISE2, nomY = input$variableCROISE1)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1,nom = input$variableCROISE1, nomY = input$variableCROISE2)}
      res$Descriptif
    }, caption = "Descriptif global et par modalité",
    caption.placement = getOption("xtable.caption.placement", "bottom"),
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)

    output$descr3TestNormalite<- renderPrint({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
      print(res[2])
    })
    output$descr3Testpv<- renderPrint({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
      print(res[3])
    })
    output$descr3TestsNPv<- renderPrint({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
      print(res[4])
    })

    output$descr3Tests_de_Student<- renderPrint({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
      print(res[5])
    })


    output$descr3TestsMANN<- renderPrint({
      base    <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){res<-descr3(variableCROISE1,variableCROISE2)}
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){res<-descr3(variableCROISE2,variableCROISE1)}
      print(res[6])
    })



    output$CorrelationCROISE<- renderPrint({
      base            <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]
      x<-variableCROISE1
      y<-variableCROISE2
      resultatCorrelation<-
        cat("Coefficient de corrélation de Pearson\n Le coefficient de corrélation linéaire de Pearson (Rho) est estimé à",
            round(cor.test(x,y,method = "pearson" )$estimate,3),
            "et son intervalle de confiance à 95% est: [",
            round(cor.test(x,y)$conf.int[1],3),";",round(cor.test(x,y)$conf.int[2],3),
            "] \n La p.valeur associée au test de nullité de ce coefficient est estimée à:",
            round(cor.test(x,y,method = "pearson") $p.value,4),"\n\n\n\n",
            "Coefficient de corrélation de Spearman\n Le coefficient de corrélation non-paramétrique de Spearman est estimé à:",
            round(cor.test(x,y,method="s")$estimate,3),
            " \n La p.valeur associée au test de nullité de ce coefficient est estimée à:",
            round(cor.test(x,y,method = "s")$p.value,4),"\n\n",
            "\n\n\n Il n'est pas préconisé d'utiliser un test plutôt qu'un autre. Le test de Pearson est un test de corrélation linéaire entre les deux mesures. Le test de Spearman est un test non-paramétrique, plutôt adapté à des relations non-linéaires entre les variables.
      \n\n Le graphique ci-dessous représente l'éventuelle corrélation de ces deux variables et l'intervalle de confiance qui lui est associé."
        )

    })

    output$ChoixSortieCROISE<- renderPrint({
      base            <-r$BDD
      variableCROISE1 <-base[,colnames(base)==input$variableCROISE1]
      variableCROISE2 <-base[,colnames(base)==input$variableCROISE2]


      ChoixTest<-function(Y,X){

        nom<-deparse(substitute(X))
        if(!is.factor(X)){X<-as.factor(X)}
        nomY<-deparse(substitute(Y))
        nbnv<-nlevels(X)

        if(nlevels(X)==2){pvaleur<-format.pval(wilcox.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvaleur<-format.pval(kruskal.test(Y~X)$p.value,digits=4)}}

        if(nlevels(X)==2){testnp<-paste("Test de Mann & Whitney : p =",pvaleur)}else{if(nlevels(X)>2){testnp<-paste("Test de Kruskal & Wallis : p =",pvaleur)}}

        pvalstud<-matrix(c(NA,NA),ncol=1)
        rownames(pvalstud)<-c("Test de Student, variances egales   : p =","Test de Student, variances inegales : p =")
        colnames(pvalstud)<-c("")

        if(nlevels(X)==2)
        {pvalstud[1]<-t.test(Y~X,var.equal = TRUE)$p.value}
        else
        {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}

        if(nlevels(X)==2)
        {pvalstud[2]<-t.test(Y~X,var.equal = FALSE)$p.value}
        else
        {if(nlevels(X)>2){pval<-format.pval(summary(aov(Y~X))[[1]][1,5],digits=4)}}


        if(nlevels(X)==2){testp<-round(pvalstud,digits=4)}else{if(nlevels(X)>2){testp<-paste("Analyse de la Variance : p =",pval)}}

        if(nlevels(X)==2){pvartest<-format.pval(var.test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvartestpg<-format.pval(bartlett.test(Y~X)$p.value,digits=4)}}

        if(nlevels(X)==2){testpv<-paste(list(paste("Test parametrique d'egalite de deux variances (Fisher): p =",pvartest)
        ))
        }else{if(nlevels(X)>2){testpv<-paste("Test parametrique d'egalite de plus de deux variances (Bartlett) : p =",pvartestpg)}}

        pvalnorm<-matrix(c(NA,NA),ncol=1)
        rownames(pvalnorm)<-c("Test de normalite de Shapiro-Wilk       : p =","Test de normalite de Kolmogorov-Smirnov : p =")
        colnames(pvalnorm)<-c("")
        if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value}else{pvalnorm[1]<-NA}
        pvalnorm[2]<-ks.test(Y,"pnorm",mean(Y,na.rm=T),sd(Y,na.rm=T))$p.value
        pvalnorm<-round(pvalnorm,digits=4)

        if(nlevels(X)==2){pvalfl2g<-format.pval(ansari_test(Y~X)$p.value,digits=4)}else{if(nlevels(X)>2){pvalfl3g<-format.pval(fligner_test(Y~X)$p.value,digits=4)}}

        if(nlevels(X)==2){testnpv<-paste(list(paste("Test non param. d'egalite de deux variances (Ansari) : p =",pvalfl2g)
        ))
        }else{if(nlevels(X)>2){testnpv<-paste("Test non param. d'egalite de plus de deux variances (Fligner) : p =",pvalfl3g)}}


        ############# le choix	#########
        if(length(Y)<5000){pvalnorm[1]<-shapiro.test(Y)$p.value

        if(nlevels(X)==2){
          if(pvalnorm[1]>0.05){
            if(pvartest>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser un test de Student - variances égales"}else{choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser un test deStudent - variances inégales"}
          }else{
            if(pvalfl2g>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les distributions d'utiliser le test de Mann-Whitney"}else{choix<-"La distribution des données est irrégulière dans les groupes, aucun des tests ne permet ici une comparaison des groupes. Les hypothèses du test de Mann-Whitney ne sont pas vérifiées."}
          }# fin else shapiro
        }# fin levels =2

        if(nlevels(X)>2){
          if(pvalnorm[1]>0.05){
            if(pvartestpg>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser l'ANOVA moyennes égales"}else{choix<-"D'après la distribution des données, il est préconisé pour comparer les moyennes d'utiliser l'ANOVA moyennes inégales"}
          }else{
            if(pvalfl3g>0.05){choix<-"D'après la distribution des données, il est préconisé pour comparer les distributions d'utiliser le test de Kruskal-Wallis"}else{choix<-"La distribution des données est irrégulière dans les groupes, aucun des tests ne permet ici une comparaison des groupes. Les hypothèses du test de Mann-Whitney ne sont pas vérifiées"}
          }# fin else shapiroe RIEN
        }# fin levels =2
        return(choix)
        }else{# fin si moins de 5000
          res<-"Les effectifs sont très importants, il est préconisé ici d'utiliser le test de Student pour comparer les moyennes."
        }
      }# fin function

      if(input$qualiquantiCROISE1=="quant" & input$qualiquantiCROISE2=="qual"){
        res<-ChoixTest(variableCROISE1,variableCROISE2)
      }
      if(input$qualiquantiCROISE1=="qual" & input$qualiquantiCROISE2=="quant"){
        res<-ChoixTest(variableCROISE2,variableCROISE1)
      }
      res
    })


    # ####################### Tableau de croisement ###########################
    # 
    output$propositionsTableauCROISE <- renderUI({

      selectInput(ns("VariableCroisement"), "Variable de croisement:",   choices=r$noms[r$nbModeVariable<5])
    })
    


    #/home/tibo/Bureau/Shiny/BDD.csv
    # Variable Quanti "normale"

    })

    observe({
      if(sum( as.logical(r$variableNormale), na.rm = T)==0){
        listeVariableNormale <-  ""
      }else{ listeVariableNormale =c(r$noms[which(r$nbModeVariable>1 & r$variableNum & as.logical(r$variableNormale))],"")
      }
      r$listeVariableNormale<-listeVariableNormale
    })


    observe({
      if(sum( !as.logical(r$variableNormale), na.rm = T)==0){
        listeVariableNonNormale <-  ""
      }else{ listeVariableNonNormale =c(r$noms[which(r$nbModeVariable>1 & r$variableNum & !as.logical(r$variableNormale))],"")
      }
      r$listeVariableNonNormale<-listeVariableNonNormale
    })

    observe({
    output$selectionVariablesCroisees1 <- renderUI({

      selectInput(ns("VariableCroisees1"), "Variables Croisées Quantitatives (moyenne et écart-type)",
                  choices =  list(`Variables Normales` =r$listeVariableNormale,
                                  `Variables non Normales` = r$listeVariableNonNormale)
                  ,
                  selected = NULL,
                  multiple = TRUE    )
    })

    output$selectionVariablesCroisees3 <- renderUI({

      selectInput(ns("VariableCroisees3"), "Variables Croisées Quantitatives (médiane, 1er et 3ème quartiles)",
                  choices =  list(`Variables non Normales` = r$listeVariableNonNormale,
                                  `Variables Normales` =r$listeVariableNormale)
                  ,
                  selected = NULL,
                  multiple = TRUE    )
    })

    output$selectionVariablesCroisees2 <- renderUI({

      selectInput(ns("VariableCroisees2"), "Variables Croisées Qualitatives (pourcentage et effectif)",
                  choices = r$noms[r$nbModeVariable<13& r$nbModeVariable>1],
                  selected = NULL,
                  multiple = TRUE    )
    })
    })



    observe({
      base<- r$BDD
      if(is.null(input$VariableCroisees1)&is.null(input$VariableCroisees2) & is.null(input$VariableCroisees3)){
        # if(is.null(input$VariableCroisees1)){  tableauCroisement<- cbind( base%>% select(input$VariableCroisees2)%>% mutate_all(factor))
        # } else if(is.null(input$VariableCroisees2)){  tableauCroisement<- cbind(base%>% select(input$VariableCroisees1))
      }else{
        #
        #   variablecommune<- input$VariableCroisees2[input$VariableCroisees2%in% input$VariableCroisees1]
        #   if(!length(variablecommune)==0){
        #     variablecommune2<- paste(variablecommune,"qual")
        #     base[,variablecommune2]<- base[,variablecommune]
        #     variabletableau2<- c(input$VariableCroisees2[!input$VariableCroisees2%in% input$VariableCroisees1],variablecommune2)
        #   }else{
        #     variabletableau2<-input$VariableCroisees2
        #   }
        #   tableauCroisement<- cbind(base%>% select(input$VariableCroisees1), base%>% select(variabletableau2)%>% mutate_all(factor))
        # }

        #Création tableau de croisement des variables à distributions normales
        if(!is.null(input$VariableCroisees1)){
          tableauCroisement1 <-  cbind(base%>% select(input$VariableCroisees1))
          names(tableauCroisement1) <-paste( input$VariableCroisees1, "(moyenne (sd))")
          names(tableauCroisement1) <-paste(names(tableauCroisement1),"(Na=", apply(tableauCroisement1, 2,function(x) sum(is.na(x))),")", sep = "")

          tableauCroisement1<-tableauCroisement1 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
            desctable ::desctable(stats = list("mean_p" = is.factor ~ percent | mean,
                                               "sd" = is.factor ~ length | sd),tests =tests_autoGMRC ) %>%
            as.data.frame
          colonne<- (dim(tableauCroisement1)[2]-3)/2
          tableauCroisement1Sortie <- tableauCroisement1[, c(1, (c(1:colonne)* 2),dim(tableauCroisement1)[2]-1, dim(tableauCroisement1)[2]) ]
          for(i in 1 :colonne ){
            tableauCroisement1Sortie[,(1+i)] <- paste(round(tableauCroisement1[,(i*2)],input$nbDec)," (",round(tableauCroisement1[,(i*2+1)],input$nbDec),")", sep= "")
          }

          names(tableauCroisement1Sortie)[1]= "Variables"
        }else{tableauCroisement1Sortie<-NULL}



        if(!is.null(input$VariableCroisees2)){tableauCroisement2 <-  cbind(base%>% select(input$VariableCroisees2)%>% mutate_all(factor))
        # names(tableauCroisement2) <-paste( input$VariableCroisees2, "(pourcentage(nombre))")
        names(tableauCroisement2) <-paste(names(tableauCroisement2),"\n (Na=", apply(tableauCroisement2, 2,function(x) sum(is.na(x))),")", sep = "")

        tableauCroisement2<-tableauCroisement2 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
          desctable ::desctable(stats = list("mean_p" = is.factor ~ percent | mean,
                                             "sd" = is.factor ~ length | sd),tests =tests_autoGMRC ) %>%
          as.data.frame
        colonne<- (dim(tableauCroisement2)[2]-3)/2
        tableauCroisement2Sortie <- tableauCroisement2[, c(1, (c(1:colonne)* 2),dim(tableauCroisement2)[2]-1, dim(tableauCroisement2)[2]) ]
        for(i in 1 :colonne ){

          tableauCroisement2Sortie[,(1+i)] <- paste(round(tableauCroisement2[,(i*2)],input$nbDec),"% (",round(tableauCroisement2[,(i*2+1)],input$nbDec),")")
        }
        # aRajouter<-substr(tableauCroisement2Sortie[1,],regexpr(":",tableauCroisement2Sortie[1,])[1],nchar(tableauCroisement2Sortie[1,]))
        # aRajouter<- ""
        tableauCroisement2Sortie[,1] <-gsub(" Groupe","",tableauCroisement2Sortie[,1])
        tableauCroisement2Sortie[,1] <- paste(gsub(".*:","Groupe :",tableauCroisement2Sortie[,1]),"(pourcentage(effectif))" )
        tableauCroisement2Sortie[,1] <-gsub(") .*",")(effectif)",tableauCroisement2Sortie[,1])
        for( i in 2: (dim(tableauCroisement2Sortie)[2]-2)){
          tableauCroisement2Sortie[,i] <- gsub("NA %","",tableauCroisement2Sortie[,i])
        }
        names(tableauCroisement2Sortie)[1]= "Variables"
        }else{tableauCroisement2Sortie<-NULL}


        #Création tableau de croisement des variables à distributions normales mediane / 1er et 3eme quartiles
        if(!is.null(input$VariableCroisees3)){
          tableauCroisement3 <-  cbind(base%>% select(input$VariableCroisees3))
          names(tableauCroisement3) <-paste( input$VariableCroisees3, "(mediane(1er-3ème quartiles))")
          names(tableauCroisement3) <-paste(names(tableauCroisement3),"\n (Na=", apply(tableauCroisement3, 2,function(x) sum(is.na(x))),")", sep = "")
          tableauCroisement3<-tableauCroisement3 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
            desctable ::desctable(stats = list("mean_p" = median,
                                               "Q1" = function(x) round(quantile(x,0.25),input$nbDec), "Q3"= function(x) round(quantile(x,0.75),input$nbDec)),tests =tests_autoGMRC ) %>%
            as.data.frame
          colonne<- (dim(tableauCroisement3)[2]-3)/3
          tableauCroisement3Sortie <- tableauCroisement3[, c(1, (c(1:colonne)* 3-1),dim(tableauCroisement3)[2]-1, dim(tableauCroisement3)[2]) ]
          for(i in 1 :colonne ){
            tableauCroisement3Sortie[,(1+i)] <- paste(round(tableauCroisement3[,(i*3)-1],input$nbDec),"
                                                  (",tableauCroisement3[,(i*3)], "-", tableauCroisement3[,(i*3)+1],")", sep="")
          }

          names(tableauCroisement3Sortie)[1]= "Variables"

        }else{tableauCroisement3Sortie<-NULL}


        tableauCroisementSortie<- rbind(tableauCroisement1Sortie,tableauCroisement3Sortie,tableauCroisement2Sortie)

        nomColonne<-names(tableauCroisementSortie)

        nomColonne<- gsub("tests /","",nomColonne)
        nomColonne<- gsub("/ ","",nomColonne)
        nomColonne<- gsub("\\_p.*","",nomColonne)
        nomColonne<- gsub("sd.*","",nomColonne)
        nomColonne<- gsub("mean","",nomColonne)
        nomColonne<-gsub(".*:",paste(input$VariableCroisement,":"),nomColonne)
        # nomColonne<-gsub("\\base","",nomColonne)
        names(tableauCroisementSortie)<- nomColonne

        # tableauCroisement2<-tableauCroisement1 %>%group_by(as.factor(base[,input$VariableCroisement]))%>%
        #   desctable ::desctable(stats = list("mean_p" = is.factor ~ percent | mean,
        #                                      "sd" = is.factor ~ length | sd),tests =tests_autoGMRC ) %>%
        #
        #   as.data.frame
        # names(tableauCroisemen2)[1]= "Variables"
        #
        colp<-dim(tableauCroisementSortie)[2]-1
        tableauCroisementSortie[, colp]<-ifelse( tableauCroisementSortie[, colp]<1/10^(input$nbDec),paste("<",as.character(1/10^(input$nbDec)),sep=""), round(tableauCroisementSortie[, colp],input$nbDec))
        tableauCroisementSortie[,colp+1]<- gsub("%>%","",tableauCroisementSortie[,colp+1])
        if (input$tableauCroiseSimpli ==1){
          tableauCroisementSortie[,1]<- gsub("\\(moyenne \\(sd\\)\\)","*",tableauCroisementSortie[,1])
          tableauCroisementSortie[,1]<- gsub("\\(mediane\\(1er-3ème quartiles\\)\\)","°",tableauCroisementSortie[,1])
          tableauCroisementSortie[,1]<- gsub("\\(pourcentage\\(effectif\\)\\)","§",tableauCroisementSortie[,1])
          tableauCroisementSortie[,1]<- gsub("\\(effectif\\)","§",tableauCroisementSortie[,1])


        }
        r$tableCroise<-tableauCroisementSortie


      }

    })

    observe({

    output$downloadData <- downloadHandler(
      filename ="Tableau Croisement.csv",



      content = function(file) {



        write.csv2(r$tableCroise, file)
      }
    )

    output$tableauCroisement <- renderTable({
      r$tableCroise


    }, na = "", digits = 3)

    output$legende <- renderUI({

      if (input$tableauCroiseSimpli ==1){
        affichage<-HTML("* moyenne (sd)
          <br/>° mediane (1er-3ème quartile)
          <br/>§ pourcentage (effectif)")
        affichage
      }

    })
  })
  })
}
    
## To be copied in the UI
# mod_Croisements_ui("Croisements_1")
    
## To be copied in the server
# mod_Croisements_server("Croisements_1")
