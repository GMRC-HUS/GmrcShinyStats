#' SaisieManuelle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList


mod_SaisieManuelle_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
  dashboardPage(dashboardHeader(title = "Saisie manuelle"),
                dashboardSidebar(
                  sidebarMenu(
                    conditionalPanel(
                      condition = "!input.sidebarCollapsed",
                    numericInput(ns("NbLignesMAIN"), tags$p("Nombre de lignes",style = "color:white"), 2, min = 1),
                    numericInput(ns("NbcolonnesMAIN"), tags$p("Nombre de colonnes",style = "color:white"), 2, min = 1),
                    tags$p("   Entrez les valeurs agrégées du"),
                    tags$p("tableau, séparées par un espace"),
                    tags$p("(remplissage en colonne)."),
                    textInput(ns("TableauMAIN1"), label = tags$p("Effectifs du tableau:",style = "color:white"), value = "1 2 3 4")
                  ))
                ),
                dashboardBody(fluidPage(
                  #titlePanel("Réaliser un tableau croisé"),
                  tags$h3("Réaliser un tableau croisé",align = "center",style = "color:#08088A; font-family: Georgia; font-size : 30px;"),
                  conditionalPanel(
                    condition = "input.sidebarCollapsed",
                    tags$p(tags$b("Pour modifier la saisie de votre tableau, dépliez la barre latérale."), style="font-size:17px; color:coral")),

                       tags$h3("Tableau croisé",align = "left",style = "color:#08088A"),
                       tags$p("On présente ci-dessous le tableau croisé des deux variables:"),
                       textOutput(ns("msgTableau")),
                       fluidRow(
                        splitLayout(cellWidths = c("30%","30%","30%"), 
                                    tableOutput(ns('montableauCroisemanuel')), 
                                    tableOutput(ns('montableauCroisemanuel2')),
                                    tableOutput(ns('montableauCroisemanuel3'))
                                    
                        )
                      ),# fin fluid row
                      tags$h3("Tests d'association / Comparaison des proportions",align = "left",style = "color:#08088A"),
                      fluidRow(
                        splitLayout(cellWidths = c("50%","50%"), 
                                    tableOutput(ns('MAINtableCHI2')), 
                                    tableOutput(ns('MAINtableFISHER'))
                        )
                      ),
                      
                      # spécialités pour les tableaux 2x2
                      conditionalPanel(
                        condition = "input.NbLignesMAIN=='2' && input.NbcolonnesMAIN=='2'",
                        ns=ns,
                        textOutput(ns('CHI2conditions')),
                        tags$h3("Rapport de cotes / Différence de proportions",align = "left",style = "color:#08088A"),
                        fluidRow(
                          splitLayout(cellWidths = c("50%","50%"), 
                                      tableOutput(ns('oddratioMAIN')), 
                                      tableOutput(ns('DiffDesProps'))
                          )
                        ),
                        tags$h3("Performance diagnostique",align = "left",style = "color:#08088A"),
                        tags$p("Le prochain tableau présente les critères de performances diagnostiques de X sur la valeur de Y"),
                        tableOutput(ns('PerforMAIN'))
                        
                      ),#fin conditional panel des tableaux 2x2
                      
                      
                      tags$h3("Concordance / Kappa",align = "left",style = "color:#08088A"),
                      tags$p("La concordance est évaluée à l'aide du coefficient Kappa de Cohen, et est estimé ci-dessous:"),
                      fluidRow(
                        splitLayout(cellWidths = c("25%","55%","20%"), 
                                    tableOutput(ns('KappaMAIN')), 
                                    plotOutput(ns('KappaPlotMain')),
                                    tableOutput(ns('LandisEtKoch'))
                        )# fin SpliLayout Kappa
                        
                      )# fin fluiRow pour Kappa
                      
                       # fin Main panel
                     # fin sidebarlayout
                ))
  )
  
  
}
    
#' SaisieManuelle Server Functions
#'
#' @noRd 
mod_SaisieManuelle_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #source("./CodeSansDependance.R", local = TRUE)
    #source("./fonctions.R", local = TRUE)
    #source("./miseEnForme.R", local = TRUE)
    #eval(parse("./miseEnForme.R", encoding="UTF-8"))
    #eval(parse("./CodeSansDependance.R", encoding="UTF-8"))
    
    
    ########################################################################################################################
    ####    SAISIE MANUELLE ONGLET 1
    ########################################################################################################################

    matriceMAIN <- reactive({
      table_crossed(input$NbLignesMAIN, input$NbcolonnesMAIN, input$TableauMAIN1)
    })

    observeEvent(matriceMAIN(), ignoreInit = TRUE, {
      Mat <- matriceMAIN()
      if (is.null(Mat)) {
        return(invisible(NULL))
      }
      ch2p <- tryCatch(suppressWarnings(chisq.test(Mat, correct = FALSE)$p.value),
                       error = function(e) NA)
      fishe <- tryCatch(suppressWarnings(fisher.test(Mat)$p.value),
                       error = function(e) NA)
      enregistrer_resultat(r, "Saisie manuelle",
                           paste("Tableau croisé", dim(Mat)[1], "×", dim(Mat)[2]),
                           paste("χ² p =", round(ch2p, 3),
                                 "; Fisher p =", round(fishe, 3)))
    })

    observeEvent(c(input$Concoman1, input$Concoman2), ignoreInit = TRUE, {
      if (is.null(input$Concoman1) || is.null(input$Concoman2)) {
        return(invisible(NULL))
      }
      if (input$Concoman1 == "" || input$Concoman2 == "") {
        return(invisible(NULL))
      }
      x <- as.factor(strsplit(input$Concoman1, " ")[[1]])
      y <- as.factor(strsplit(input$Concoman2, " ")[[1]])
      if (length(x) != length(y) || length(x) == 0) {
        return(invisible(NULL))
      }
      Mat2 <- cbind(x, y)
      Mat2 <- Mat2[complete.cases(Mat2), ]
      if (nrow(Mat2) == 0) {
        return(invisible(NULL))
      }
      kk <- tryCatch(suppressWarnings(kappa2(Mat2)), error = function(e) NULL)
      if (is.null(kk)) {
        return(invisible(NULL))
      }
      enregistrer_resultat(r, "Saisie manuelle",
                           "Concordance entre deux lecteurs (saisie manuelle)",
                           paste("Kappa =", round(kk$value, 3),
                                 "; p =", round(kk$p.value, 3),
                                 "; interprétation :", interpretation_kappa(kk$value)))
    })

    output$msgTableau <- renderText({
      if(is.null(matriceMAIN()))
        "Le tableau est invalide : vérifiez le nombre de lignes/colonnes et saisissez des entiers positifs séparés par un espace (remplissage par colonne)."
      else ""
    })

    output$montableauCroisemanuel <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      Matrice0   <- matriceMAIN(); if(is.null(Matrice0)) return(NULL)
      Matrice    <- addmargins(Matrice0)
      colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
      rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
      Matrice },digits=0, caption = "Tableau des effectifs",
      caption.placement = getOption("xtable.caption.placement", "bottom"), 
      caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)
    
    output$montableauCroisemanuel2 <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      Matrice0   <- matriceMAIN(); if(is.null(Matrice0)) return(NULL)
      Matrice    <- round(addmargins(100 * prop.table(addmargins(Matrice0, 1), 1), 2), 2)
      colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
      rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
      Matrice
    }, caption = "Pourcentages ligne",
    caption.placement = getOption("xtable.caption.placement", "bottom"), 
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)  
    
    output$montableauCroisemanuel3 <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      Matrice0   <- matriceMAIN(); if(is.null(Matrice0)) return(NULL)
      Matrice    <- round(addmargins(100 * prop.table(addmargins(Matrice0, 2), 2), 1), 2)
      colnames(Matrice)<-c(  paste("Y",1:Nbcolonnes -1 ) , "Total")
      rownames(Matrice)<-c(  paste("X",1:Nblignes -1 ) , "Total")
      Matrice
    }, caption = "Pourcentages colonne",
    caption.placement = getOption("xtable.caption.placement", "bottom"), 
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)
    
    output$MAINtableCHI2 <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
      
      CH2<-chisq.test(Mat,correct=FALSE)
      resTESTS<-cbind(CH2$statistic,CH2$parameter,CH2$ p.value)
      colnames(resTESTS)<-c("CHI2 Stat","CHI2 Degrés","CHI2 pValue")
      rownames(resTESTS)<-"Résultat"
      resTESTS
    },rownames=TRUE)  
    
    output$MAINtableFISHER <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
      
      FI2<-fisher.test(Mat)
      resTESTS<-t(t( FI2$ p.value))
      colnames(resTESTS)<-c("Fisher pValue")
      rownames(resTESTS)<-"Résultat"
      resTESTS
    },rownames=TRUE)  
    
    
    output$CHI2conditions <- renderText({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      
      Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
      
      CH2<-chisq.test(Mat,correct=FALSE)
      FI2<-fisher.test(Mat)
      ifelse(all(CH2$expected>5),"Au vu des effectifs théoriques >5, on préfèrera ici l'utilisation du test du Chi2",
             "Au vu des faibles effectifs théoriques, on préfèrera ici l'utilisation du test exact de Fisher")
    })   
    
    output$oddratioMAIN <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      if(Nblignes>2 | Nbcolonnes>2){OR<-NULL}else{
        Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
        FI2<-fisher.test(Mat)
        OR<-cbind(FI2$ estimate , FI2$ conf.int[[1]],FI2$ conf.int[[2]])
        colnames(OR)<-c("Rapport de cotes","Borne inf 2.5","Borne Sup 97.5")
        rownames(OR)<-"Résultat"}
      OR
    }, caption = "Rapport de cotes et IC",
    caption.placement = getOption("xtable.caption.placement", "bottom"), 
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)   
    
    
    
    output$PerforMAIN <- renderTable({
      
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      if(Nblignes>2 | Nbcolonnes>2){res<-NULL}else{
        Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
        
        x<-rep(c(0,1),c(Mat[1,1]+Mat[1,2],Mat[2,1]+Mat[2,2]))
        y<-rep(c(0,1,0,1),c(Mat[1,1],Mat[1,2],Mat[2,1],Mat[2,2]))
        LL<-logist(y,x)
        LLV<-data.frame(LL$Valeurs)
        Se<-LLV$VP/(LLV$VP+LLV$FN)
        Sp<-LLV$VN/(LLV$VN+LLV$FP)
        VPP<-LLV$VP/(LLV$VP+LLV$FP)
        VPN<-LLV$VN/(LLV$VN+LLV$FN)
        res<-round(data.frame(Sens=Se,Spec=Sp,VPP=VPP,VPN=VPN)*100,2)  
        rownames(res)<-"Résultat"}
      res
    },rownames=TRUE)   
    
    output$DiffDesProps <- renderTable({
      
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      if(Nblignes>2 | Nbcolonnes>2){Res<-NULL}else{
        Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
        ICdiff<-IC.diff.prop(Mat[1,2], Mat[1,1]+Mat[1,2],Mat[2,2], Mat[2,1]+Mat[2,2], alpha01 = 0.5, alpha02 = 0.5, beta01 = 0.5,beta02 = 0.5, val = 0.95)
        Res<-100*cbind(ICdiff$Estimation,ICdiff$IC[3,1],ICdiff$IC[3,2])
        colnames(Res)<-c("Différence de proportions","Borne 2.5","Borne 97.5")
        rownames(Res)<-"Résultat"
      }
      Res
    }, caption = "Différence proportions et IC",
    caption.placement = getOption("xtable.caption.placement", "bottom"), 
    caption.width = getOption("xtable.caption.width", NULL),rownames=TRUE)     
    
    
    
    output$KappaMAIN <- renderTable({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      
      Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
      # une fonction pour transfo la table contingence en BDD
      countsToCases <- function(x, countcol = "Freq") {
        idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
        x[[countcol]] <- NULL
        x[idx, ]
      }
      # Mat 2 les donnees en BDD
      Mat2<-countsToCases(as.data.frame.table(Mat))
      
      if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
        lkappa.boot <- function(data,x) {suppressWarnings(kappa2(data[x,]))$value}
        res <- boot(Mat2,lkappa.boot,1000)
        RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
      }    
      res<-round(data.frame(Kappa=RESULTAT[1],Ic2.5=RESULTAT[2],Ic97.5=RESULTAT[3],pval=suppressWarnings(kappa2(cbind(Mat2[,1],Mat2[,2])))$p.value),3)  
      rownames(res)<-"Résultat"
      res
    },rownames=TRUE)   
    
    output$KappaPlotMain <- renderPlot({
      Nblignes   <-input$NbLignesMAIN
      Nbcolonnes <-input$NbcolonnesMAIN
      
      Mat0<-matriceMAIN(); if(is.null(Mat0)) return(NULL)
      Mat<-Mat0
      # une fonction pour transfo la table contingence en BDD
      countsToCases <- function(x, countcol = "Freq") {
        idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
        x[[countcol]] <- NULL
        x[idx, ]
      }
      # Mat 2 les donnees en BDD
      Mat2<-countsToCases(as.data.frame.table(Mat))
      
      lkappa.boot <- function(data,x) {suppressWarnings(kappa2(data[x,]))$value}
      res <- boot(Mat2,lkappa.boot,1000)
      
      # Function to plot color bar
      color.bar <- function(lut, min, max=-min, nticks=11,ticks=seq(min, max, len=nticks), title='') {
        scale = (length(lut)-1)/(max-min)
        
        
        plot(c(0,0), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', ylim=c(-0.1,1.1),main=title)
        axis(1, ticks, las=1)
        for (i in 1:(length(lut)-1)) {
          x = (i-1)/scale + min
          rect(x,0,x+1/scale,10 ,col=lut[i], border=NA)
        }
        segments( res$t0,-0.1, res$t0,1.5,lwd=8)
      }
      
      color.bar(colorRampPalette(c("red", "yellow", "yellow", "blue"))(100), -1)
      
    },height=150)   
    
    
    ##### Saisi manuelle seule : 
    output$mytableCONCORDANCE2 <- renderTable({
      
      if( is.null(input$Concoman1)) return()
      if(input$Concoman1 =="" |! length(strsplit(input$Concoman1," ")[[1]])== length(strsplit(input$Concoman2," ")[[1]])) return()
      
      variableCONCORDANCE1 <-as.factor(strsplit(input$Concoman1," ")[[1]])
      variableCONCORDANCE2 <-as.factor(strsplit(input$Concoman2," ")[[1]])
      
      as.data.frame.matrix(addmargins(table(variableCONCORDANCE1,variableCONCORDANCE2)))
    },rownames=TRUE)
    
    
    output$ConcordanceManuelleINTERV2 <- renderPrint({
      if(input$Concoman1 =="") return()
      if (! length(strsplit(input$Concoman1," ")[[1]])== length(strsplit(input$Concoman2," ")[[1]])) return("Les variables doivent avoir la même longueur")
      
      x <-as.factor(strsplit(input$Concoman1," ")[[1]])
      y <-as.factor(strsplit(input$Concoman2," ")[[1]])
      
      # creation matrice
      Mat2<-cbind(x,y)
      Mat2<-Mat2[complete.cases(Mat2),]
      
      # partage des facteurs
      Mat2						<-as.data.frame(Mat2)
      LEV						<-sort(unique(c(as.character(levels(as.factor(Mat2[,1]))),as.character(levels(as.factor(Mat2[,2]))))))
      Mat2[,1]				<-as.factor(Mat2[,1])
      Mat2[,2]				<-as.factor(Mat2[,2])
      levels(Mat2[,1])	<- c(levels(Mat2[,1]),LEV[!is.element(LEV,levels(Mat2[,1]))]      )
      levels(Mat2[,2])	<- c(levels(Mat2[,2]),LEV[!is.element(LEV,levels(Mat2[,2]))]      )
      Mat2[,1]<-reorder_factor_levels(Mat2[,1], new.order=levels(Mat2[,2]))
      
      if(all(Mat2[,1]==Mat2[,2])){RESULTAT<-c(1,1,1)}else{
        lkappa.boot <- function(data,x) {suppressWarnings(kappa2(data[x,]))$value}
        res <- boot(Mat2,lkappa.boot,10000)
        RESULTAT<-c(lkappa.boot(Mat2),boot.ci(res,type="bca")$ bca[,4:5])
      }
      cat("Le coefficient de concordance Kappa de Cohen est estimé à",RESULTAT[1],
          "dans l'intervalle à 95% [",RESULTAT[2],";",RESULTAT[3],"]\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(suppressWarnings(kappa2(cbind(x,y)))$p.value,3), "\n")
    })
    
    output$ConcordanceManuelleSimple2 <- renderPrint({ 
      if(input$Concoman1 =="") return()
      if (! length(strsplit(input$Concoman1," ")[[1]])== length(strsplit(input$Concoman2," ")[[1]])) return("Les variables doivent avoir la même longueur")
      
      
      x <-as.factor(strsplit(input$Concoman1," ")[[1]])
      y <-as.factor(strsplit(input$Concoman2," ")[[1]])
      cat("Estimation\nLe coefficient de concordance Kappa de Cohen est estimé à",round(suppressWarnings(kappa2(cbind(x,y)))$value,3),".
      \n\nTest\nLe test de nullité de ce coefficient peut être réalisé et la p.valeur associée est",round(suppressWarnings(kappa2(cbind(x,y)))$p.value,3), "\n")
    })
    
    output$LandisEtKoch <- renderTable({
      data.frame(Kappa=c("0-0.2","0.21-0.40","0.41-0.60","0.61-0.80","0.81-1"),
                 interpretation=c("très faible","faible","modéré","fort","presque parfait"))
      
    },rownames=TRUE)  
    
  })
}
    
## To be copied in the UI
# mod_SaisieManuelle_ui("SaisieManuelle_1")
    
## To be copied in the server
# mod_SaisieManuelle_server("SaisieManuelle_1")
