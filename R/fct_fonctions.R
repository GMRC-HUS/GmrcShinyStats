#' fonctions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


formule<- function(x){
  
  return(as.formula(paste("~", x)))
}

barplot_croise<-function(base,var1,var2){
  BDD<- base[,c(var1,var2)]
  pourcent <-  prop.table(table(BDD),1)
  data<-as.data.frame(pourcent)
  names(data)<- c("var1","var2","Freq")
  data[,1]<-as.factor(data[,1])
  data[,2]<-as.factor(data[,2])
  maxPourcent<- max(data$Freq, na.rm = T)
  label<-  paste(round(data$Freq,3)*100,"%")
  vjust<- unlist(as.list(ifelse(data$Freq< maxPourcent/5, -1.6, 1.6)), use.names = F)
  
  barplotCroise <- ggplot(data=data, aes(x=var2 ,y=Freq))+
    geom_col( position = "dodge",color='black',aes(fill = var2))+
    facet_wrap( ~var1)+
    geom_text(data=data,aes( label = paste(round(Freq,3)*100,"%")) , vjust=vjust, color="black", size=5) +
    theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))+
    ggtitle(paste("En fonction de ", var1, sep = ""))+
    xlab(var2)+
    labs(fill = var2)
  return(barplotCroise)
}

tablePourcent<- function(base){
  pourcent <-  prop.table(table(base)) 
  pourcent<- pourcent[order(pourcent)]
  
  data<- data.frame(pourcent = as.numeric(pourcent), nom = names(pourcent))
  
  
  
  return(data)
  
}

pieChart<- function(base){
  data<- tablePourcent(base)
  bp<- ggplot(data=data, aes(x=0 ,y=pourcent, fill=reorder_factor_levels(factor(nom), nom[order(-pourcent)])))+
    coord_polar(theta='y')
  df <- try(data %>% mutate(pos = cumsum(sort(data$pourcent))- sort(data$pourcent)/2))
  
  if(length(df)>0){
    label <- (sort(round(data$pourcent*100,1)))
    label <- as.character(ifelse(label<4,"",paste(label ,"%")))
    
    nom <- data$nom[order(data$pourcent,decreasing = F)]
    
    y<- df$pos
    
    pie <- bp +
      labs(title="Diagramme circulaire", 
           x="", y = "")+
      theme(axis.text.x=element_blank())+
      geom_bar(stat="identity", color='black')+
      guides(fill=guide_legend(override.aes=list(colour="black")))+
      theme(axis.ticks=element_blank(), 
            axis.title=element_blank(), 
            axis.text.y=element_blank()) +
      
      theme_void()+
      geom_text(aes( x= 0.2,y=df$pos, label = label), size=6) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))
    
    
    pie$labels$fill <- ""
    pie$theme$legend.title$size <- 15
    return(pie)
    
  }else{
    return("une erreur c'est produite")
  }
}


diagrammeBarre <- function(base){
  data<- tablePourcent(base)
  bp<-ggplot(data=data, aes(x=nom ,y=pourcent*100, fill=reorder_factor_levels(factor(nom), nom[order(-pourcent)])))
  
  maxPourcent<- max(data$pourcent, na.rm = T)
  label<-  paste(round(data$pourcent,3)*100,"%")
  vjust<- unlist(as.list(ifelse(data$pourcent< maxPourcent/5, -1.6, 1.6)), use.names = F)
  
  barre <- bp +
    labs(title="Diagramme en barre", 
         x="", y = "pourcentage")+
    geom_bar(stat="identity", color='black')+
    guides(fill=guide_legend(override.aes=list(colour=NULL)))+
    
    
    geom_text(aes( label = label), vjust=vjust, color="black", size=5) +
    theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=17))
  
  barre$labels$fill <- ""
  return(barre)
  
}

t.testVarEgal<- function(x,...){
  t.test(x,var.equal = T,...)
}

tests_autoGMRC<-function (var, grp){
  grp <- grp %>% factor
  if (nlevels(grp) < 2) 
    ~no.test
  else if (var %>% is.factor) 
    if ( tryCatch(stats::chisq.test(var , grp)$p.value>=0 , warning = function(e) F, error = function(e) F))
      ~ chisq.test
  else ~fisher.test
  else {
    all_normal <- all(var %>% tapply(grp, desctable::is.normal))
    if (nlevels(grp) == 2) 
      if (all_normal) 
        if (tryCatch(stats::var.test(var ~ grp)$p.value > 
                     0.1, warning = function(e) F, error = function(e) F)) 
          ~t.testVarEgal
    else ~. %>% t.test(var.equal = F)
    else ~wilcox.test
    else if (all_normal) 
      if (tryCatch(stats::bartlett.test(var ~ grp)$p.value > 
                   0.1, warning = function(e) F, error = function(e) F)) 
        ~. %>% oneway.test(var.equal = T)
    else ~. %>% oneway.test(var.equal = F)
    else ~kruskal.test
  }
}

reorder_factor_levels <- function(x, new.order) {
  lv <- levels(x)
  new_levels <- c(intersect(new.order, lv), setdiff(lv, new.order))
  factor(x, levels = new_levels)
}

file.choose2 <- function(...) {
  pathname <- NULL;
  tryCatch({
    pathname <- file.choose(T);
  }, error = function(ex) {
  })
  pathname;
}